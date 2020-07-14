open Core
open Async
open Yojson.Basic.Util

open Cohttp_async

module Wm = struct
  module Rd = Webmachine.Rd
  module UnixClock = struct
    let now = fun () -> int_of_float (Unix.gettimeofday ())
  end
  include Webmachine.Make(Cohttp_async.Io)(UnixClock)
end

class gkback conf_init = object(self)
  inherit [Body.t] Wm.resource

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
    let _ = Map.find configurations (self#configuration) in
    let json =
      Printf.sprintf "hej"
    in
    Wm.continue (`String json) rd
end
;;

let webpath = (Sys.get_argv ()).(1)
;;

let deassoc_string = (fun (assoc_string) ->
    match assoc_string with (`String s) -> s)
;;

let read_layer_metadata (layer) =
  Yojson.Basic.from_file
    (String.concat [webpath; "/layers/"; layer; "/meta.json"])
;;
let read_configuration configfolder configuration =
  let conftype = configuration |> member "type" |> to_string in
  if equal_string conftype "single"
  then
    (* With a single-layer configuration, we simply use the metadata for the layer,
     * and add the type and layer address from the configuration *)
    let conflayeraddress = configuration |> member "layer" |> to_string in
    let layermeta = to_assoc (read_layer_metadata conflayeraddress) in
    let assocconf = to_assoc configuration in
    (configfolder, Yojson.Basic.to_string (`Assoc (List.concat [layermeta; assocconf])))
  else if equal_string conftype "multi"
  then
    (* With a multi-layer configuration, we expect the necessary
     * meta-data to be written explicitly in the configuration *)
    (configfolder, Yojson.Basic.to_string configuration)
  else
    raise (Invalid_argument "Invalid configuration type. Must be either \"single\" or \"multi\"")
;;

let read_configurations (configurationpath) =
  (* Read configurations from filesystem *)
  Sys.ls_dir configurationpath
  >>| List.map ~f:(fun (configfolder) ->
          read_configuration configfolder
            (Yojson.Basic.from_file
               (String.concat
                  [configurationpath; "/"; configfolder; "/configuration.json"])))
  (* Build map of json-formatted configurations *)
  >>| List.fold_left
        ~init:(Map.empty (module String))
        ~f:(fun confmap configurationtuple ->
          let (configfolder, configuration) = configurationtuple in
          (* let _ = (* Write to file *) in *)
          Map.add_exn confmap ~key:configfolder ~data:configuration)
;;

let main () =
  let port = 8080 in
  let webpath = (Sys.get_argv ()).(1) in
  let configurations = read_configurations (String.concat [webpath; "/configurations"]) in
  let routes = [
      ("/mapconf/:configuration", fun () -> new gkback configurations);
    ] in
  let handler ~body _ request =
    let open Cohttp in
    Wm.dispatch' routes ~body ~request
    >>| begin function
          | None        -> (`Not_found, Header.init (), `String "Not found", [])
          | Some result -> result
        end

    >>= fun (status_code, headers, body, _) ->
    Server.respond ~headers ~body status_code
  in
  Server.create ~on_handler_error:`Raise (Tcp.Where_to_listen.of_port port) handler
  >>> (fun _server ->
    Log.Global.info "webpath: %s, gkback: listening on 0.0.0.0:%d%!" webpath port)
;;

Scheduler.go_main ~main ()
