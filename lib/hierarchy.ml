open Core

module StringMap = Map.Make(String)

let rec build_hierarchy_helper (root : string) parent_mp=
  let osm = Osm_xml.Parser.parse_file ("data/boundaries/"^root^".osm") in
  let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.relations in
  let relations = Map.to_alist (relations_of_osm osm) in
  let rec explore_children list mp=
  match list with
  | [] -> mp
  | ((Osm_xml.Types.OSMId child) ,Osm_xml.Types.OSMRelation _)::xs -> explore_children xs (build_hierarchy_helper child (Map.set mp ~key:child ~data:root)) in
  explore_children relations parent_mp

let build_hierarchy (root : string) = 
  build_hierarchy_helper root StringMap.empty

