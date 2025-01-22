open Core

module StringMap = Map.Make(String)

exception TagNotFound of string * string

(** given a root, return a map of key child and value parent for all boundaries contained in root. mp[root]="0-0-root"*)
let build_hierarchy (root_level : string) (root_id : string) (root_name : string) = 
  (* given a root, get its children boundaries from the osm file, set parent_mp[child]=root and recurse down child if first time seen*)
  let rec build_hierarchy_helper (root_level : string) (root_id : string) (root_name : string)  parent_mp=
    let osm_file = ("data/boundaries/"^root_level^"-"^root_id^"-"^root_name^".osm") in
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file osm_file in
    Map.fold osm_record.relations ~init:parent_mp ~f:(fun ~key:(Osm_xml.Types.OSMId child_id) ~data:(Osm_xml.Types.OSMRelation child_relation) mp ->
      match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
      | None -> raise (TagNotFound ("admin_level",child_id))
      | Some child_level -> 
        begin match Osm_xml.Types.find_tag child_relation.tags "name" with 
        | None -> raise (TagNotFound ("name",child_id))
        | Some child_name -> 
          begin match Map.find mp (child_level^"-"^child_id^"-"^child_name) with
          | None -> 
            build_hierarchy_helper child_level child_id child_name  (StringMap.add_exn mp ~key:(child_level^"-"^child_id^"-"^child_name) ~data:(root_level^"-"^root_id^"-"^root_name))
          | Some prv_parent -> 
            begin match String.split_on_chars ~on:['-'] prv_parent with 
            | prv_parent_level::_->
              if int_of_string prv_parent_level >= int_of_string root_level then mp else
                StringMap.set mp ~key:(child_level^"-"^child_id^"-"^child_name) ~data:(root_level^"-"^root_id^"-"^root_name)
            | _ -> raise (Invalid_argument prv_parent) 
            end
          end
        end) in
  let parent_mp = StringMap.add_exn StringMap.empty ~key:(root_level^"-"^root_id^"-"^root_name) ~data:"0-0-root" in
  build_hierarchy_helper root_level root_id root_name parent_mp

(** inverts a string to string map, returning a string to string list map*)
let invert_map mp = 
  Map.fold mp ~init:StringMap.empty ~f:(fun ~key:k ~data:v acc -> 
    (Map.update acc v ~f:(function
    | None -> [k] 
    | Some values -> (k :: values)
  )))