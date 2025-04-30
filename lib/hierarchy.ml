open Core


exception TagNotFound of string * string

(** inverts a string to string map, returning a string to string list map*)
let invert_map_list mp = 
    Map.fold mp ~init:String.Map.empty ~f:(fun ~key:k ~data:v acc -> 
      (Map.update acc v ~f:(function
      | None -> [k]
      | Some values -> k::values
    )))

(* let invert_list_map_list mp = 
    Map.fold mp ~init:String.Map.empty ~f:(fun ~key:k ~data:l acc -> 
        List.fold l ~init:acc ~f:(fun acc v->
            (Map.update acc v 
                ~f:(function
                | None -> [k]
                | Some values -> k::values
                )
            )
        )
    ) *)
  
let invert_map_set mp = 
    Map.fold mp ~init:String.Map.empty ~f:(fun ~key:k ~data:v acc -> 
      (Map.update acc v ~f:(function
      | None -> String.Set.singleton k 
      | Some values -> Set.add values k
    )))

(** given a root, return a map of key child and value parent for all boundaries contained in root. mp[root]="0-0-root"*)
let boundary_to_parent (root_level : string) (root_id : string) (root_name : string) = 
    (* given a root, get its children boundaries from the osm file, set parent_mp[child]=root and recurse down child if first time seen*)
    let rec helper (root_level : string) (root_id : string) (root_name : string)  parent_mp=
        let root_string = root_level^"-"^root_id^"-"^root_name in
        let osm_file = ("data/"^root_string^".osm") in
        let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file osm_file in
        Map.fold osm_record.relations ~init:parent_mp ~f:(fun ~key:(Osm_xml.Types.OSMId child_id) ~data:(Osm_xml.Types.OSMRelation child_relation) mp ->
        match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
        | None -> mp
        | Some child_level -> 
            begin match Osm_xml.Types.find_tag child_relation.tags "name" with 
            | None -> raise (TagNotFound ("name",child_id))
            | Some child_name -> 
                let child_string = child_level^"-"^child_id^"-"^child_name in
                begin match Map.find mp child_string with
                | None -> 
                    helper child_level child_id child_name  (Map.add_exn mp ~key:child_string ~data:root_string)
                | Some prv_parent -> 
                    begin match String.split_on_chars ~on:['-'] prv_parent with 
                    | prv_parent_level::_->
                    if int_of_string prv_parent_level >= int_of_string root_level then mp else
                        Map.set mp ~key:child_string ~data:root_string
                    | _ -> raise (Invalid_argument prv_parent) 
                    end
                end
            end) in
    let parent_mp = Map.add_exn String.Map.empty ~key:(root_level^"-"^root_id^"-"^root_name) ~data:"0-0-root" in
    helper root_level root_id root_name parent_mp

let idseen_buildingswithnostreet_streetnametowayidsandbuildings_outernames boundary_string id_seen= 
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/"^boundary_string^".osm") in
    let relation_id_tags = Map.fold osm_record.relations ~init:[] ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMRelation relation_record) l -> (("relation "^id), relation_record.tags)::l) in 
    let relation_ways_id_tags = Map.fold osm_record.ways ~init:relation_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMWay way_record) l -> (("way "^id),way_record.tags)::l) in 
    let relation_ways_nodes_id_tags = Map.fold osm_record.nodes ~init:relation_ways_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMNode node_record) l -> (("node "^id), node_record.tags)::l) in 
    List.fold relation_ways_nodes_id_tags ~init:(id_seen, String.Set.empty, String.Map.empty, String.Set.empty) ~f:(fun (id_seen, buildings_with_no_street, street_name_to_way_ids_and_buildings, outer_names) (id, record) -> 
        if Set.mem id_seen id then (id_seen, buildings_with_no_street, street_name_to_way_ids_and_buildings, outer_names)
        else
            match Osm_xml.Types.find_tag record "building" with
                | Some _-> 
                    begin match Osm_xml.Types.find_tag record "addr:street" with
                    | Some street -> 
                        let building_name = 
                            begin match Osm_xml.Types.find_tag record "name" with
                            | Some name -> name
                            | None ->
                                begin match Osm_xml.Types.find_tag record "addr:housenumber" with
                                | Some house_number -> house_number^" "^street
                                | None -> raise (TagNotFound ("name and addr:housenumber",id))
                                end
                            end in
                        (
                            Set.add id_seen id, 
                            buildings_with_no_street, 
                            begin Map.update street_name_to_way_ids_and_buildings street ~f:(function
                            | None -> ([], String.Set.singleton building_name)
                            | Some (way_ids, buildings) -> (way_ids, Set.add buildings building_name))
                            end, 
                            outer_names
                        )
                    | None -> 
                        begin match Osm_xml.Types.find_tag record "name" with
                        | Some name -> 
                            (
                                Set.add id_seen id, 
                                Set.add buildings_with_no_street name, 
                                street_name_to_way_ids_and_buildings,
                                outer_names
                            )
                        | None -> raise (TagNotFound ("name and addr:street",id))
                        end
                    end
                | None ->
                    begin match Osm_xml.Types.find_tag record "admin_level" with
                    | Some _->
                        (
                            Set.add id_seen id, 
                            buildings_with_no_street, 
                            street_name_to_way_ids_and_buildings, 
                            outer_names
                        )
                    | None ->
                        if String.is_prefix id ~prefix:"node"
                            then 
                                (
                                    id_seen, 
                                    buildings_with_no_street, 
                                    street_name_to_way_ids_and_buildings, 
                                    Set.add outer_names id
                                )
                        else
                            begin match Osm_xml.Types.find_tag record "highway" with
                            | None -> raise (TagNotFound ("highway",id))
                            | Some _ -> 
                                let street_name = 
                                    begin match Osm_xml.Types.find_tag record "name" with
                                    | Some street_name -> street_name
                                    | None -> 
                                        begin match Osm_xml.Types.find_tag record "ref" with
                                        | Some street_name -> street_name
                                        | None-> id
                                        end
                                    end in
                                (
                                    id_seen, buildings_with_no_street, 
                                    begin Map.update street_name_to_way_ids_and_buildings street_name ~f:(function
                                    | None -> ([id], String.Set.empty)
                                    | Some (way_ids, buildings) -> (id::way_ids, buildings))
                                    end, 
                                    outer_names
                                )
                            end
                    end
    )

let way_id_to_junctions boundary_string id_seen outer_names= 
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/"^boundary_string^".osm") in
    let streets = Map.filter osm_record.ways 
        ~f:(fun (Osm_xml.Types.OSMWay way_record) ->
            match Osm_xml.Types.find_tag way_record.tags "highway" with
            | None -> false
            | Some _ -> true) in
    let (id_seen, node_to_street_name_to_way_id) =
        Map.fold 
            streets
            ~init:(id_seen, String.Map.empty) 
            ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMWay way_record) (id_seen, node_to_street_name_to_way_id) -> 
                let street_id = "way "^id in
                if Set.mem id_seen street_id then (id_seen, node_to_street_name_to_way_id) 
                else
                    let id_seen = Set.add id_seen street_id in
                    let street_name = 
                        match Osm_xml.Types.find_tag way_record.tags "name" with
                        | Some name -> name
                        | None -> 
                            begin match Osm_xml.Types.find_tag way_record.tags "ref" with
                            | Some ref -> ref
                            | None -> street_id
                            end in
                    List.fold way_record.nodes ~init:(id_seen, node_to_street_name_to_way_id) 
                        ~f:(fun (id_seen, node_to_street_name_to_way_id) (Osm_xml.Types.OSMId node_id) -> 
                            let node_id = "node "^node_id in
                            let id_seen =
                                if Set.mem outer_names node_id then
                                    Set.remove id_seen street_id
                                else id_seen in
                            (
                                id_seen,
                                Map.update node_to_street_name_to_way_id node_id ~f:(function
                                | None -> String.Map.singleton street_name street_id
                                | Some mp -> Map.set mp ~key:street_name ~data:street_id)
                            )
                        )
            ) in
    (
        id_seen,
        Map.fold node_to_street_name_to_way_id ~init:String.Map.empty 
            ~f:(fun ~key:node_id ~data:street_name_to_way_id street_id_to_junctions->
                if (Map.length street_name_to_way_id)>1 || Set.mem outer_names node_id then
                    Map.fold street_name_to_way_id ~init:street_id_to_junctions
                        ~f:(fun ~key:_ ~data:way_id street_id_to_junctions ->
                            Map.update street_id_to_junctions way_id ~f:(function
                            | None -> [node_id]
                            | Some l -> node_id::l)
                        )
                else street_id_to_junctions
            )
    )