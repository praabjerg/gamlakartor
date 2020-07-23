(* Copyright 2020  Palle Raabjerg *)

(* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Oldmap_j

let read_file filepath =
  let channel = open_in filepath in
  let filestring = really_input_string channel (in_channel_length channel) in
  let _ = close_in channel in
  filestring

let read_meta () = maplayer_of_string (read_file "meta.json")

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


let translate_and_warp meta =
  let _ = if Array.length Sys.argv < 3 then
            let _ = print_string (String.concat " " ["Usage:"; Sys.argv.(0); "points_file input_mapimage\n\nExecute in the directory where you want the layer. Make sure meta.json is present. The process will use /tmp as temporary storage for intermediate files.\n"]) in
            exit 0
  in
  let points_filename = Sys.argv.(1) in
  let mapimage_filename = Sys.argv.(2) in
  let resolution = (string_of_float meta.resolution) in
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
  let warp_command = String.concat " " ["gdalwarp -tps -r bilinear -s_srs \"EPSG:4326\" -t_srs \"EPSG:3857\" -overwrite -tr"; resolution; resolution; "-co TILED=YES /tmp/translated.tiff"; "/tmp/projected.tiff"] in
  let () = print_string (String.concat "" [warp_command; "\n"]) in
  Sys.command warp_command

let convert_to_tiles meta =
  let minzoom = (string_of_int meta.minZoom) in
  let maxzoom = (string_of_int meta.maxZoom) in
  let gdal2tiles_command = String.concat "" ["gdal2tiles.py --webviewer=oldmap --zoom="; minzoom; "-"; maxzoom; " /tmp/projected.tiff ./"] in
  let () = print_string (String.concat "" [gdal2tiles_command; "\n"]) in
  Sys.command gdal2tiles_command

let main () =
  let meta = read_meta() in
  let _ = translate_and_warp meta in
  convert_to_tiles meta

let _ = main ()
