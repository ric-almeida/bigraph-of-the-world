open Lwt.Syntax
open Cohttp_lwt
open Cohttp_lwt_unix
open Core

let api = "https://overpass-api.de/api/interpreter"
(*"https://overpass.osm.jp/api/interpreter"*)
(*"https://maps.mail.ru/osm/tools/overpass/api/interpreter"*)
(*"https://overpass.private.coffee/api/interpreter"*)

(*exception DownloadFailed of  Cohttp.Code.status_code*)

(** downloads from uri to file with name dest*)
let rec download (uri : Uri.t) (dest : string) =
  let* resp, body = Client.get uri in
  match resp.status with
  | `OK -> let stream = Body.to_stream body in
  Lwt_io.with_file ~mode:Lwt_io.output dest (fun chan ->
      Lwt_stream.iter_s (Lwt_io.write chan) stream)
  | _ -> print_endline ("Retrying download for "^dest); download uri dest
  
(** downloads osm file of all buildings in area defined by query_relation*)
let queryBuildings (admin_level : string) (query_relation : string) (name :string)=
  let url = api^"?data=relation%28"^query_relation^"%29%3Bmap%5Fto%5Farea%3Bnwr%5B%22name%22%5D%5B%22addr%3Astreet%22%5D%28area%29%3Bout%20meta%3B%0A" in
  download (Uri.of_string url) ("data/buildings/"^admin_level^"-"^query_relation^"-"^name^".osm")

(** downloads osm file of all admin boundaries in area defined by query_relation. If there are no child boundaries, call queryBuildings*)
let queryBoundaries (admin_level : string) (query_relation : string) (name : string)=
  let url = api^"?data=rel%28"^query_relation^"%29%3B%0Amap_to_area%3B%0Arelation%5B%22boundary%22%3D%22administrative%22%5D%28area%29%28if%3A%20t%5B%22admin_level%22%5D%20%3E%20"^admin_level^"%29%3B%0Aout%20meta%3B%0A" in
  Lwt.bind (download (Uri.of_string url) ("data/boundaries/"^admin_level^"-"^query_relation^"-"^name^".osm")) (fun ()->
    let osm = Osm_xml.Parser.parse_file ("data/boundaries/"^admin_level^"-"^query_relation^"-"^name^".osm") in
    let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.relations in
    let relations = relations_of_osm osm in
    match Map.is_empty relations with
    | true -> queryBuildings admin_level query_relation name
    | false -> Lwt.return_unit)

exception TagNotFound of string

(** downloads osm file of all admin boundaries in area defined by query_relation, and call queryBoundaries on all child boundaries*)
let queryAllChildren (admin_level : string) (query_relation : string) (name :string) =
  let url = api^"?data=rel%28"^query_relation^"%29%3B%0Amap_to_area%3B%0Arelation%5B%22boundary%22%3D%22administrative%22%5D%28area%29%28if%3A%20t%5B%22admin_level%22%5D%20%3E%20"^admin_level^"%29%3B%0Aout%20meta%3B%0A" in
  Lwt_main.run (download (Uri.of_string url) ("data/boundaries/"^admin_level^"-"^query_relation^"-"^name^".osm")); 
  let osm = Osm_xml.Parser.parse_file ("data/boundaries/"^admin_level^"-"^query_relation^"-"^name^".osm") in
  let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.relations in
  let relations = Map.to_alist (relations_of_osm osm) in
  match List.is_empty relations with
  | true-> queryBuildings admin_level query_relation name
  | false-> Lwt_list.iter_s (fun ((Osm_xml.Types.OSMId child) ,Osm_xml.Types.OSMRelation osm_relation_record)-> let a_level= Osm_xml.Types.find_tag osm_relation_record.tags "admin_level" in
    match a_level with
    | None -> raise (TagNotFound query_relation) 
    | Some level -> let name= Osm_xml.Types.find_tag osm_relation_record.tags "name" in
      match name with 
      | None -> raise (TagNotFound child)
      | Some name ->queryBoundaries level child name) relations