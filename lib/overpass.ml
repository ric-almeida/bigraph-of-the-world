open Lwt.Syntax
open Core

let api = "https://overpass-api.de/api/interpreter"
(*"https://overpass.osm.jp/api/interpreter"*)
(*"https://maps.mail.ru/osm/tools/overpass/api/interpreter"*)
(*"https://overpass.private.coffee/api/interpreter"*)

(** writes string to file*)
let write_to_file body file_path = 
  let f ch = Lwt_stream.iter_s (Lwt_io.write ch) (Lwt_stream.of_list [body]) in
  Lwt_io.with_file ~mode:Lwt_io.output file_path f

(** downloads from uri to file with name dest*)
let rec download (uri : Uri.t) (file_path : string) =
  let* resp, body = Cohttp_lwt_unix.Client.get uri in
  match resp.status with
  | `OK -> 
    let stream = Cohttp_lwt.Body.to_stream body in
    let f ch = Lwt_stream.iter_s (Lwt_io.write ch) stream in
    Lwt_io.with_file ~mode:Lwt_io.output file_path f
  | _ -> 
    let _ = print_endline ("Retrying download for "^file_path) in
    download uri file_path
  
(** downloads osm file of all buildings in area defined by root_id*)
let query_buildings (root_level : string) (root_id : string) (root_name :string)=
  let url = api^"?data=relation%28"^root_id^"%29%3Bmap%5Fto%5Farea%3Bnwr%5B%22name%22%5D%5B%22addr%3Astreet%22%5D%28area%29%3Bout%20meta%3B%0A" in
  let file_path = "data/buildings/"^root_level^"-"^root_id^"-"^root_name^".osm" in
  download (Uri.of_string url) file_path

(** downloads osm file of all admin boundaries in area defined by root_id. If there are no child boundaries, call queryBuildings*)
let query_boundaries (root_level : string) (root_id : string) (root_name : string)=
  let url = api^"?data=rel%28"^root_id^"%29%3B%0Amap_to_area%3B%0Arelation%5B%22boundary%22%3D%22administrative%22%5D%28area%29%28if%3A%20t%5B%22admin_level%22%5D%20%3E%20"^root_level^"%29%3B%0Aout%20meta%3B%0A" in
  Lwt.bind (download (Uri.of_string url) ("data/boundaries/"^root_level^"-"^root_id^"-"^root_name^".osm")) (fun _->
    let osm_file = "data/boundaries/"^root_level^"-"^root_id^"-"^root_name^".osm" in
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file osm_file in
    match Map.is_empty osm_record.relations with
    | true -> query_buildings root_level root_id root_name
    | false -> Lwt.return_unit)

exception TagNotFound of string * string

(** downloads osm file of all admin boundaries in area defined by root_id, and call queryBoundaries on all child boundaries*)
let query_all_children (root_level : string) (root_id : string) (root_name :string) =
  let url = api^"?data=rel%28"^root_id^"%29%3B%0Amap_to_area%3B%0Arelation%5B%22boundary%22%3D%22administrative%22%5D%28area%29%28if%3A%20t%5B%22admin_level%22%5D%20%3E%20"^root_level^"%29%3B%0Aout%20meta%3B%0A" in
  let osm_file = "data/boundaries/"^root_level^"-"^root_id^"-"^root_name^".osm" in
  let _ = download (Uri.of_string url) osm_file in 
  let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file osm_file in
  match Map.is_empty osm_record.relations with
  | true-> query_buildings root_level root_id root_name
  | false->
    let f (Osm_xml.Types.OSMId child_id, Osm_xml.Types.OSMRelation child_relation) =
      match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
      | None -> raise (TagNotFound ("admin_level", child_id))
      | Some child_level ->
        match Osm_xml.Types.find_tag child_relation.tags "name" with 
        | None -> raise (TagNotFound ("name",child_id))
        | Some child_name -> query_boundaries child_level child_id child_name in
    Lwt_list.iter_s f (Map.to_alist osm_record.relations)