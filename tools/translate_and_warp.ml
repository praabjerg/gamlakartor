let translate_set labels values =
  let gcparray = Array.make 4 "" in
  let () = List.iter2 (fun label value ->
      match label with
      | "mapX" -> Array.set gcparray 2 value
      | "mapY" -> Array.set gcparray 3 value
      | "pixelX" -> Array.set gcparray 0 value
      | "pixelY" -> Array.set gcparray 1 (string_of_float ((float_of_string value) *. -1.))
      | _ -> ())
             labels values
  in
  String.concat " " ("-gcp"::(Array.to_list gcparray))


let translate_and_warp () =
  let _ = if Array.length Sys.argv < 5 then
            let _ = print_string (String.concat " " ["Usage:"; Sys.argv.(0); "points_file input_mapimage output_tiff resolution\n"]) in
            exit 0
  in
  let points_filename = Sys.argv.(1) in
  let mapimage_filename = Sys.argv.(2) in
  let output_filename = Sys.argv.(3) in
  let resolution = Sys.argv.(4) in
  let translate_prefix = "gdal_translate -of GTiff" in
  let points_channel = open_in points_filename in
  let points_csv = Csv.of_channel points_channel in
  let labels = Csv.next points_csv in
  let translate_main = Csv.fold_left ~f:(fun command gcprecord -> String.concat " " [command; (translate_set labels gcprecord)])
                  ~init:translate_prefix points_csv
  in
  let translate_command = String.concat " " [translate_main; mapimage_filename; "/tmp/translated.tiff"] in
  let () = print_string (String.concat "" [translate_command; "\n"]) in
  let _ = Sys.command translate_command in
  let warp_command = String.concat " " ["gdalwarp -tps -r bilinear -s_srs \"EPSG:4326\" -t_srs \"EPSG:3857\" -overwrite -tr"; resolution; resolution; "-co TILED=YES /tmp/translated.tiff"; output_filename] in
  let () = print_string (String.concat "" [warp_command; "\n"]) in
  Sys.command warp_command

let _ = translate_and_warp ()
