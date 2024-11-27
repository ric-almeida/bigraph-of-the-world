open Lwt.Syntax
open Cohttp_lwt
open Cohttp_lwt_unix
open Core

let download (uri : Uri.t) (dest : string) =
  let* _resp, body = Client.get uri in
  let stream = Body.to_stream body in
  Lwt_io.with_file ~mode:Lwt_io.output dest (fun chan ->
      Lwt_stream.iter_s (Lwt_io.write chan) stream)

let queryBuildings (query_relation : string) =
  let url = "https://overpass-api.de/api/interpreter?data=relation%28"^query_relation^"%29%3Bmap%5Fto%5Farea%3Bnwr%5B%22building%22%5D%5B%22name%22%5D%5B%22addr%3Astreet%22%5D%28area%29%3Bout%20meta%3B%0A" in
  download (Uri.of_string url) ("data/buildings/"^query_relation^".osm")

let queryBoundaries (query_relation : string) (admin_level : string) =
  let url = "https://overpass-api.de/api/interpreter?data=relation%28"^query_relation^"%29%3Bmap%5Fto%5Farea%3Brelation%5B%22boundary%22%3D%22administrative%22%5D%5B%22admin%5Flevel%22%21%3D%22"^admin_level^"%22%5D%28area%29%3Bout%20meta%3B%0A" in
  Lwt.bind (download (Uri.of_string url) ("data/boundaries/"^query_relation^".osm")) (fun ()->
    let osm = Osm_xml.Parser.parse_file ("data/boundaries/"^query_relation^".osm") in
    let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.relations in
    let relations = relations_of_osm osm in
    match Map.is_empty relations with
    | true -> queryBuildings query_relation
    | false -> Lwt.return_unit)

let queryAllChildren (query_relation : string) (admin_level : string)=
  let url = "https://overpass-api.de/api/interpreter?data=relation%28"^query_relation^"%29%3Bmap%5Fto%5Farea%3Brelation%5B%22boundary%22%3D%22administrative%22%5D%5B%22admin%5Flevel%22%21%3D%22"^admin_level^"%22%5D%28area%29%3Bout%20meta%3B%0A" in
  Lwt_main.run (download (Uri.of_string url) ("data/boundaries/"^query_relation^".osm")); 
  let osm = Osm_xml.Parser.parse_file ("data/boundaries/"^query_relation^".osm") in
  let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.relations in
  let relations = Map.to_alist (relations_of_osm osm) in
  match List.is_empty relations with
  | true-> queryBuildings query_relation
  | false-> Lwt_list.iter_p (fun ((Osm_xml.Types.OSMId child) ,Osm_xml.Types.OSMRelation osm_relation_record)-> let a_level= Osm_xml.Types.find_tag osm_relation_record.tags "admin_level" in
    match a_level with
    | None -> Lwt.return_unit
    | Some level -> queryBoundaries child level) relations