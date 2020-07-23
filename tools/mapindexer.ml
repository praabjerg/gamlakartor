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

let read_maplayers layerids = List.map read_maplayer layerids

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
  Html.(li ~a:[a_class ["mapentry"; "entry"]] [
            build_thumb configid config.thumb_alt;
            build_generic layer.title "entrytitle" "Titel";
            build_source layer.sourceLink layer.sourceName;
            build_generic layer.year "mapyear" "Årstal";
            build_generic layer.projectedBy "credit" "Projekterad av";
            build_generic layer.uploaded "uploaded" "Uppladdad";
            build_gcp layerid;
  ])

let build_entry_multi configid config layerids layers =
  match config.multititle with
  | Some multititle ->
     Html.(li ~a:[a_class ["mapentry"; "entry"]] [
               build_thumb configid config.thumb_alt;
               build_generic multititle "entrytitle" "Titel";
               ul ~a:[a_class ["layerlist"]]
                 (List.map2 (fun layerid layer ->
                      li ~a:[a_class["layerentry"; "entry"]] [
                          (let layertitle = match layer.layerTitle with
                             | Some title ->
                                title
                             | None ->
                                String.concat "" [layer.place; ", "; layer.year]
                           in
                           build_generic layertitle "layertitle" "Skikt");
                          build_source layer.sourceLink layer.sourceName;
                          build_generic layer.year "mapyear" "Årstal";
                          build_generic layer.projectedBy "credit" "Projekterad av";
                          build_generic layer.uploaded "uploaded" "Uppladdad";
                          build_gcp layerid;
                    ])
                    layerids layers)
     ])
  | _ -> failwith (String.concat " " ["Multi map-configuration"; configid; "missing \"multititle\" field."])

let build_entry configid =
  let map_config = read_mapconf configid in
  if map_config.conftype = "single" then
    match map_config.layer with
    | Some layerid -> build_entry_single configid map_config layerid (read_maplayer layerid)
    | _ -> failwith (String.concat " " ["Single map-configuration"; configid; "missing layer reference."])
  else if map_config.conftype = "multi" then
    match map_config.layers with
    | Some layerids -> build_entry_multi configid map_config layerids (read_maplayers layerids)
    | _ -> failwith (String.concat " " ["Single map-configuration"; configid; "missing layer reference."])
  else
    failwith "Only \"single\" and \"multi\" map type configurations possible."

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
  close_out file_handle

let () = writeindex ()
