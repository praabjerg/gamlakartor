open Lwt.Infix
open Yojson.Basic.Util
open Cohttp_lwt_unix

module Wm = struct
  module Rd = Webmachine.Rd
  module UnixClock = struct
    let now = fun () -> int_of_float (Unix.gettimeofday ())
  end
  include Webmachine.Make(Cohttp_lwt_unix__Io)(UnixClock)
end

module StringMap = Map.Make(String)

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
    (configfolder, Yojson.Basic.to_string (`Assoc (List.concat [layermeta; assocconf])))
  else if conftype = "multi"
  then
    (* With a multi-layer configuration, we expect the necessary
     * meta-data to be written explicitly in the configuration *)
    (configfolder, Yojson.Basic.to_string configuration)

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
  (* Build map of json-formatted configurations *)
  List.fold_left
    (fun confmap configurationtuple ->
      let (configfolder, configuration) = configurationtuple in
      StringMap.add configfolder configuration confmap)
    StringMap.empty
    configlist
;;

let main () =
  let port = 8080 in
  let configurations = read_configurations (String.concat "" [webpath; "/configurations"]) in
  let routes = [
      ("/mapconf/:configuration", fun () -> new gkback configurations);
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
