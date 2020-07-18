open Tyxml

let%html cssreset = "<link href='css/reset.css' rel='stylesheet' type='text/css'>";;
let%html cssfonts = "<link href='css/fonts.css' rel='stylesheet' type='text/css'>";;
let%html cssmain = "<link href='css/main.css' rel='stylesheet' type='text/css'>";;
let%html csskartor = "<link href='css/kartor.css' rel='stylesheet' type='text/css'>";;
let body = Html.(
    html ~a:[a_lang "se"]
      (head (title (txt "Gamla Kartor")) [
           meta ~a:[a_charset "utf-8"] ();
           cssreset;
           cssfonts;
           cssmain;
           csskartor
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
               ul []
           ]]));;
