open Core

module StringMap = Map.Make(String)

exception TagNotFound of string

(** given a root, get its children boundaries from the osm file, set parent_mp[child]=root and recurse down child if first time seen*)
let rec build_hierarchy_helper (root : string) (admin_level : string) parent_mp=
  let osm = Osm_xml.Parser.parse_file ("data/boundaries/"^admin_level^"-"^root^".osm") in
  let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.relations in
  let relations = Map.to_alist (relations_of_osm osm) in
  let rec explore_children list mp=
  match list with
  | [] -> mp
  | (Osm_xml.Types.OSMId child ,Osm_xml.Types.OSMRelation osm_relation_record)::xs -> let a_level= Osm_xml.Types.find_tag osm_relation_record.tags "admin_level" in
    match a_level with
    | None -> raise (TagNotFound child)
    | Some level -> match Map.mem mp (level^"-"^child) with
      | true  -> explore_children xs (Map.set mp ~key:(level^"-"^child) ~data:(admin_level^"-"^root))
      | false -> explore_children xs (build_hierarchy_helper child level (Map.add_exn mp ~key:(level^"-"^child) ~data:(admin_level^"-"^root)))
  in
  explore_children relations parent_mp

(** given a root, return a map of key child and value parent for all boundaries contained in root. mp[root]="0-root"*)
let build_hierarchy (root : string) (admin_level : string)= 
  build_hierarchy_helper root admin_level (Map.add_exn StringMap.empty ~key:(admin_level^"-"^root) ~data:"0-root")

