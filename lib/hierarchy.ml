open Core

module StringMap = Map.Make(String)

module StringListMap = Map.Make(String)

exception TagNotFound of string

(** given a root, get its children boundaries from the osm file, set parent_mp[child]=root and recurse down child if first time seen*)
let rec build_hierarchy_helper (admin_level : string) (rootid : string) (rootName : string)  parent_mp=
  let osm = Osm_xml.Parser.parse_file ("data/boundaries/"^admin_level^"-"^rootid^"-"^rootName^".osm") in
  let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.relations in
  let relations = Map.to_alist (relations_of_osm osm) in
  let rec explore_children list mp=
  match list with
  | [] -> mp
  | (Osm_xml.Types.OSMId child ,Osm_xml.Types.OSMRelation osm_relation_record)::xs -> let a_level= Osm_xml.Types.find_tag osm_relation_record.tags "admin_level" in
    match a_level with
    | None -> raise (TagNotFound child)
    | Some level -> if (int_of_string level) <= (int_of_string admin_level) then explore_children xs mp
    else let name= Osm_xml.Types.find_tag osm_relation_record.tags "name" in
    match name with 
    | None -> raise (TagNotFound child)
    | Some name -> match Map.find mp (level^"-"^child^"-"^name) with
      | None -> explore_children xs (build_hierarchy_helper level child name (Map.add_exn mp ~key:(level^"-"^child^"-"^name) ~data:(admin_level^"-"^rootid^"-"^rootName)))
      | Some old_parent  -> match List.hd (String.split_on_chars ~on:['-'] old_parent) with
        | None -> raise (TagNotFound child)
        | Some old_parent_level -> if (int_of_string old_parent_level) >= (int_of_string admin_level) then explore_children xs mp
          else explore_children xs (Map.set mp ~key:(level^"-"^child^"-"^name) ~data:(admin_level^"-"^rootid^"-"^rootName))
  in
  explore_children relations parent_mp

(** given a root, return a map of key child and value parent for all boundaries contained in root. mp[root]="0-0-root"*)
let build_hierarchy (admin_level : string) (rootid : string) (rootName : string) = 
  build_hierarchy_helper admin_level rootid rootName (Map.add_exn StringMap.empty ~key:(admin_level^"-"^rootid^"-"^rootName) ~data:"0-0-root")


(** inverts a string to string map, returning a string to string list map*)
let invertMap mp = 
  let rec invertMapHelper keyValueList (result_mp : string list StringListMap.t)= 
    match keyValueList with
    | [] -> result_mp
    | (k, v)::xs -> invertMapHelper xs (StringListMap.update result_mp v ~f:(function
    | None -> [k]  (* If the key doesn't exist, create a new list with the value *)
    | Some values -> (k :: values)  (* If it exists, prepend the new value to the list *)
  )) in
  invertMapHelper (Map.to_alist mp) StringListMap.empty