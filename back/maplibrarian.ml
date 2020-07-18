(* The Map Librarian builds and serves an index of projected maps for use
 * with the Gamla Kartor web frontend.
 * Copyright (C) 2020  Palle Raabjerg

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.

 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

open Lwt.Infix
open Yojson.Basic.Util
open Cohttp_lwt_unix
open Tyxml

module StringMap = Map.Make(String)

let assoclist_to_htmllist configfolder assoclist =
  let translate_generic jsontitle htmlclass spantitle =
    Html.([div ~a:[a_class [htmlclass]] [
               span [txt spantitle];
               p [txt (to_string jsontitle)]]])
  in
  let translate_thumb configfolder alt =
    Html.([div ~a:[a_class ["thumb"]] [
               a ~a:[a_href String.concat "" ["mapviewer.html?configuration="; configfolder]]
                 [];
               p [txt (to_string jsontitle)]]])
  let conftype = List.assoc "type" assoclist in
  if conftype = "single"
  then
    List.fold_left
      (fun htmllist keyvalue ->
        let (key, value) = keyvalue in
        

  List.map (fun (key, value) ->
    )

module Wm = struct
  module Rd = Webmachine.Rd
  module UnixClock = struct
    let now = fun () -> int_of_float (Unix.gettimeofday ())
  end
  include Webmachine.Make(Cohttp_lwt_unix__Io)(UnixClock)
end

class gkback conf_init = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  val configurations = conf_init

  method! allowed_methods rd =
    Wm.continue [`GET] rd

  method content_types_provided rd =
    Wm.continue [
        ("application/json", self#to_json);
      ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private configuration rd =
    try Wm.Rd.lookup_path_info_exn "configuration" rd with Caml.Not_found -> "Not found"

  method private to_json rd =
    let configuration = StringMap.find (self#configuration rd) configurations in
    Wm.continue (`String configuration) rd
end
;;

let webpath = Sys.argv.(1)
;;

let read_layer_metadata (layer) =
  Yojson.Basic.from_file
    (String.concat "" [webpath; "/layers/"; layer; "/meta.json"])
;;

let read_configuration configfolder configuration =
  let conftype = configuration |> member "type" |> to_string in
  if conftype = "single"
  then
    (* With a single-layer configuration, we simply use the metadata for the layer,
     * and add the type and layer address from the configuration *)
    let conflayeraddress = configuration |> member "layer" |> to_string in
    let layermeta = to_assoc (read_layer_metadata conflayeraddress) in
    let assocconf = to_assoc configuration in
    (configfolder, `Assoc (List.concat [layermeta; assocconf]))
  else if conftype = "multi"
  then
    (* With a multi-layer configuration, we expect the necessary
     * meta-data to be written explicitly in the configuration *)
    (configfolder, configuration)
  else
    raise (Invalid_argument "Invalid configuration type. Must be either \"single\" or \"multi\"")
;;

let read_configurations (configurationpath) =
  (* Read configurations from filesystem *)
  let configdirlist = Array.to_list (Sys.readdir configurationpath) in
  let configlist =
    List.map
      (fun (configfolder) ->
        read_configuration configfolder
          (Yojson.Basic.from_file
             (String.concat
                "" [configurationpath; "/"; configfolder; "/configuration.json"])))
      configdirlist
  in
  (* Build and write map index html file *)
  let htmllist =
    List.Map
      (fun (configurationtuple) ->
        let (configfolder, configuration) = configurationtuple in
        let title = configuration
        Html.(li ~a:[a_class ["mapentry"]] [
                  div ~a:[a_class ["entrytitle"]] [
                      span [txt "Titel"];
                      p [txt ]];
                  div ~a:[a_class ["mapdate"]] [];
                  div ~a:[a_class ["thumb"]] [];
                  div ~a:[a_class ["credit"]] [];
                  div ~a:[a_class ["gcp"]] [];
                  div ~a:[a_class ["uploaded"]] [];
                  div ~a:[a_class ["source"]] []
      ]))
  (* Build and write slim index file for serving map index *)
  let _ =
    Yojson.Basic.to_file
      (String.concat "" [webpath; "/confindex.json"])
      (`List (List.map
                (fun (configurationtuple) ->
                  let (configfolder, configuration) = configurationtuple in
                  let assocconf = to_assoc configuration in
                  let slim =
                    List.remove_assoc "layer"
                      (List.remove_assoc "maxZoom"
                         (List.remove_assoc "minZoom"
                            (List.remove_assoc "bounds"
                               (List.remove_assoc "center" assocconf))))
                  in
                  `Assoc (("id", `String configfolder)::slim)
                )
                configlist))
  in
  (* Build map of json-formatted configurations *)
  List.fold_left
    (fun confmap configurationtuple ->
      let (configfolder, configuration) = configurationtuple in
      StringMap.add configfolder (Yojson.Basic.to_string configuration) confmap)
    StringMap.empty
    configlist
;;

let main () =
  let port = 8080 in
  let configurations = read_configurations (String.concat "" [webpath; "/configurations"]) in
  let routes = [
      ("/confindex/:configuration", fun () -> new gkback configurations);
    ] in
  let callback (_ch, _conn) request body =
    let open Cohttp in
    Wm.dispatch' routes ~body ~request
    >|= begin function
          | None        -> (`Not_found, Header.init (), `String "Not found", [])
          | Some result -> result
        end
    >>= fun (status, headers, body, _) ->
    Server.respond ~headers ~body ~status ()
  in
  let conn_closed (ch, _conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let config = Server.make ~callback ~conn_closed () in
  Server.create ~mode:(`TCP(`Port port)) config >|= fun () ->
  Printf.eprintf "gkback: listening on 0.0.0.0:%d%!" port

let () = Lwt_main.run (main ())
