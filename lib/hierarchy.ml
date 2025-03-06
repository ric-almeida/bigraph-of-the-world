open Core

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

exception TagNotFound of string * string

(** given a root, return a map of key child and value parent for all boundaries contained in root. mp[root]="0-0-root"*)
let boundary_to_parent (root_level : string) (root_id : string) (root_name : string) = 
    (* given a root, get its children boundaries from the osm file, set parent_mp[child]=root and recurse down child if first time seen*)
    let rec helper (root_level : string) (root_id : string) (root_name : string)  parent_mp=
        let root_string = root_level^"-"^root_id^"-"^root_name in
        let osm_file = ("data/boundaries/"^root_string^".osm") in
        let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file osm_file in
        Map.fold osm_record.relations ~init:parent_mp ~f:(fun ~key:(Osm_xml.Types.OSMId child_id) ~data:(Osm_xml.Types.OSMRelation child_relation) mp ->
        match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
        | None -> raise (TagNotFound ("admin_level",child_id))
        | Some child_level -> 
            begin match Osm_xml.Types.find_tag child_relation.tags "name" with 
            | None -> raise (TagNotFound ("name",child_id))
            | Some child_name -> 
                let child_string = child_level^"-"^child_id^"-"^child_name in
                begin match Map.find mp child_string with
                | None -> 
                    helper child_level child_id child_name  (StringMap.add_exn mp ~key:child_string ~data:root_string)
                | Some prv_parent -> 
                    begin match String.split_on_chars ~on:['-'] prv_parent with 
                    | prv_parent_level::_->
                    if int_of_string prv_parent_level >= int_of_string root_level then mp else
                        StringMap.set mp ~key:child_string ~data:root_string
                    | _ -> raise (Invalid_argument prv_parent) 
                    end
                end
            end) in
    let parent_mp = StringMap.add_exn StringMap.empty ~key:(root_level^"-"^root_id^"-"^root_name) ~data:"0-0-root" in
    helper root_level root_id root_name parent_mp

let building_to_parent boundary_parent_to_children root = 
  let rec helper boundary parent_mp  = 
    let p = 
      match Map.find boundary_parent_to_children boundary with
      | Some children ->
        List.fold children ~init:parent_mp 
        ~f:(fun p_map child ->
          helper child p_map )
      | None -> parent_mp in
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/buildings/"^boundary^".osm") in
    let relation_id = Map.fold osm_record.relations ~init:[] ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:_ l -> ("r"^id)::l) in 
    let relation_ways_id = Map.fold osm_record.ways ~init:relation_id ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:_ l -> ("w"^id)::l) in 
    let relation_ways_nodes_id = Map.fold osm_record.nodes ~init:relation_ways_id ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:_ l -> ("n"^id)::l) in 
    List.fold relation_ways_nodes_id ~init:p ~f:(fun mp building_id -> 
      match Map.find mp building_id with
      | None -> StringMap.add_exn mp ~key:building_id ~data:boundary
      | Some _ -> mp) in
  helper root StringMap.empty

(** given boundary_level^boundary_id^boundary_name, return a map of k:street v:Map(k:building_name v:building_id) found in boundary*)
let street_to_buildings filter boundary = 
   let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/buildings/"^boundary^".osm") in
   let relation_id_tags = Map.fold osm_record.relations ~init:[] ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMRelation relation_record) l -> (("r"^id), relation_record.tags)::l) in 
   let relation_ways_id_tags = Map.fold osm_record.ways ~init:relation_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMWay way_record) l -> (("w"^id),way_record.tags)::l) in 
   let relation_ways_nodes_id_tags = Map.fold osm_record.nodes ~init:relation_ways_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMNode node_record) l -> (("n"^id), node_record.tags)::l) in 
   let filtered = List.filter relation_ways_nodes_id_tags ~f:(fun (building_id, _) -> StringSet.mem filter building_id) in
   List.fold filtered ~init:StringMap.empty ~f:(fun streets_map (building_id, building_record) -> 
      match Osm_xml.Types.find_tag building_record "addr:street" with
      | None -> raise (TagNotFound ("addr:street",building_id))
      | Some street ->
         begin match Osm_xml.Types.find_tag building_record "name" with
         | Some building_name -> 
            begin StringMap.update streets_map street ~f:(function
            | None -> StringMap.singleton building_name building_id
            | Some buildings_map -> StringMap.set buildings_map ~key:building_name ~data:building_id)
            end
         | None ->
            begin match Osm_xml.Types.find_tag building_record "addr:housenumber" with
            | Some housenumber -> 
               begin StringMap.update streets_map street ~f:(function
               | None -> StringMap.singleton (housenumber^" "^street) building_id
               | Some buildings_map -> StringMap.set buildings_map ~key:(housenumber^" "^street) ~data:building_id)
               end
            | None -> raise (TagNotFound ("name and addr:housenumber",building_id))
            end
         end
      )
   
(** inverts a string to string map, returning a string to string list map*)
let invert_map_list mp = 
  Map.fold mp ~init:StringMap.empty ~f:(fun ~key:k ~data:v acc -> 
    (Map.update acc v ~f:(function
    | None -> [k]
    | Some values -> k::values
  )))

let invert_map_set mp = 
  Map.fold mp ~init:StringMap.empty ~f:(fun ~key:k ~data:v acc -> 
    (Map.update acc v ~f:(function
    | None -> StringSet.singleton k 
    | Some values -> StringSet.add values k
  )))