open Oldmap_j
open Tyxml

let webpath = Sys.argv.(1)

let read_file filepath =
  let channel = open_in filepath in
  let filestring = really_input_string channel (in_channel_length channel) in
  let _ = close_in channel in
  filestring

let read_mapconf configid = oldmap_of_string
                           (read_file (String.concat "/" [webpath; "configurations"; configid; "configuration.json"]))

let read_maplayer layerid = maplayer_of_string
                             (read_file (String.concat "/" [webpath; "layers"; layerid; "meta.json"]))

let build_generic content htmlclass spantitle =
  Html.(div ~a:[a_class [htmlclass]] [
            span [txt spantitle];
            p [txt (content)]
  ])

let build_thumb configid alt =
  Html.(div ~a:[a_class ["thumb"]] [
            a ~a:[a_href (String.concat "" ["mapviewer.html?configuration="; configid])]
              [img ~src:(String.concat "" ["configurations/"; configid; "/thumb.png"]) ~alt:alt ()]
  ])

let build_gcp layerid =
  Html.(div ~a:[a_class ["gcp"]] [
            p [a ~a:[a_href (String.concat "/" ["layers"; layerid; "GCP.points"])] [(txt "GCP")]]
  ])

let build_source sourcelink sourcename =
  Html.(div ~a:[a_class ["source"]] [
            span [txt "Källa"];
            p [a ~a:[a_href sourcelink] [(txt sourcename)]]
  ])

let build_entry_single configid config layerid layer =
  Html.(li ~a:[a_class ["mapentry"]] [
            build_generic layer.title "entrytitle" "Titel";
            build_generic layer.year "mapyear" "Årstal";
            build_thumb configid config.thumb_alt;
            build_generic layer.projectedBy "credit" "Projekterad av";
            build_gcp layerid;
            build_generic layer.uploaded "uploaded" "Uppladdad";
            build_source layer.sourceLink layer.sourceName;
  ])

let build_entry configid =
  let map_config = read_mapconf configid in
  if map_config.conftype = "single" then
    match map_config.layer with
    | Some layerid -> build_entry_single configid map_config layerid (read_maplayer layerid)
    | _ -> failwith (String.concat " " ["Single map-configuration"; configid; "missing layer reference."])
  else
    failwith "Only single map configurations fully implemented."

let build_index () =
  let configdirlist = Array.to_list (Sys.readdir (String.concat "/" [webpath; "configurations"])) in
  List.map build_entry configdirlist

let build_html () =
  Html.(
    html ~a:[a_lang "se"]
      (head (title (txt "Gamla Kartor")) [
           meta ~a:[a_charset "utf-8"] ();
           link ~rel:[`Stylesheet] ~href:"css/reset.css" ();
           link ~rel:[`Stylesheet] ~href:"css/fonts.css" ();
           link ~rel:[`Stylesheet] ~href:"css/main.css" ();
           link ~rel:[`Stylesheet] ~href:"css/kartor.css" ();
      ])
      (body [
           div ~a:[a_id "heading"] [
               h1 [txt "Gamla Kartor"];
               nav [
                   ul [
                       li ~a:[a_class ["menuitem"]] [a ~a:[a_href "index.html"] [txt "Start"]];
                       li ~a:[a_class ["menuitem"]] [txt "Kartor"];
                       li ~a:[a_class ["menuitem"]] [a ~a:[a_href "om.html"] [txt "Om GamlaKartor.se"]]
                     ]
                 ];
             ];
           div ~a:[a_id "maplist"] [
               ul (build_index ())
  ]]))

let writeindex () =
  let file_handle = open_out (String.concat "/" [webpath; "kartor.html"]) in
  let fmt = Format.formatter_of_out_channel file_handle in
  let _ = Html.pp () fmt (build_html ()) in
  (*let _ = Format.fprintf fmt "%a@." (Html.pp ~indent:true ()) (build_html ()) in*)
  close_out file_handle

let () = writeindex ()
