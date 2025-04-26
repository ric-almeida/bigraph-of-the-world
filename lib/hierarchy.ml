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

let streetid_to_junctions boundary id_seen shared_intersections= 
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/"^boundary^".osm") in
    let streets = Map.filter osm_record.ways 
        ~f:(fun (Osm_xml.Types.OSMWay way_record) ->
            match Osm_xml.Types.find_tag way_record.tags "highway" with
            | None -> false
            | Some _ -> true) in
    let (id_seen, node_to_streets) =
        Map.fold 
            streets
            ~init:(id_seen,String.Map.empty) 
            ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMWay way_record) (id_seen,node_to_street) -> 
                let street_id = "w"^id in
                if Set.mem id_seen street_id then (id_seen,node_to_street) 
                else
                    let street_name = 
                        match Osm_xml.Types.find_tag way_record.tags "name" with
                        | Some name -> name
                        | None -> 
                            begin match Osm_xml.Types.find_tag way_record.tags "ref" with
                            | Some ref -> ref
                            | None -> street_id
                            end in
                    
                    List.fold way_record.nodes ~init:(Set .add id_seen street_id,node_to_street) 
                        ~f:(fun (id_seen, junction_to_street) (Osm_xml.Types.OSMId node_id) -> 
                            let node_id = "n"^node_id in
                            (
                                (if Set.mem shared_intersections node_id then id_seen
                                else Set.add id_seen node_id),
                                Map.update junction_to_street node_id ~f:(function
                                | None -> String.Map.singleton street_name street_id
                                | Some mp -> Map.set mp ~key:street_name ~data:street_id)
                            )
                            
                        )
            ) in
    (
        id_seen,
        Map.fold node_to_streets ~init:String.Map.empty 
            ~f:(fun ~key:node_id ~data:street_name_to_id street_id_to_junctions->
                if (Map.length street_name_to_id)>1 || Set.mem shared_intersections node_id then
                    Map.fold street_name_to_id ~init:street_id_to_junctions
                        ~f:(fun ~key:_ ~data:street_id street_id_to_junctions ->
                            Map.update street_id_to_junctions street_id ~f:(function
                            | None -> [node_id]
                            | Some l -> node_id::l)
                        )
                else street_id_to_junctions
            )
    )
    (* let (junction_to_street,_) = 
        Map.fold
            unnamed_streets
            ~init:(node_to_street,String.Map.empty)
            ~f: (fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMWay way_record) (junction_to_street,merged_junctions) ->
                let junction_id = "w"^id in
                let (junction_to_street,streets_connected,merged_junctions) = 
                    List.fold way_record.nodes ~init:(junction_to_street,String.Set.empty,merged_junctions)
                        ~f:(fun (junction_to_street,streets_connected,merged_junctions) (Osm_xml.Types.OSMId node_id) -> 
                            let node_id = "n"^node_id in
                            match Map.find junction_to_street node_id with
                            | None -> (junction_to_street,streets_connected,merged_junctions)
                            | Some l -> 
                                (Map.remove junction_to_street node_id, 
                                List.fold l ~init:streets_connected ~f:(fun streets_connected street -> Set.add streets_connected street),
                                Map.add_exn merged_junctions ~key:node_id ~data:junction_id ))
                        ) in *)
                
    

let idseen_streetnamemap_inoutintersections id_seen boundary= 
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/"^boundary^".osm") in
    let relation_id_tags = Map.fold osm_record.relations ~init:[] ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMRelation relation_record) l -> (("r"^id), relation_record.tags)::l) in 
    let relation_ways_id_tags = Map.fold osm_record.ways ~init:relation_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMWay way_record) l -> (("w"^id),way_record.tags)::l) in 
    let relation_ways_nodes_id_tags = Map.fold osm_record.nodes ~init:relation_ways_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMNode node_record) l -> (("n"^id), node_record.tags)::l) in 
    List.fold relation_ways_nodes_id_tags ~init:(id_seen, String.Map.empty, String.Set.empty) ~f:(fun (id_seen, street_name_mp, in_out_intersections) (id, record) -> 
        if Set.mem id_seen id then (id_seen, street_name_mp, in_out_intersections)
        else
            (* add to street_to_building. data is another String.Map ~key:building_name ~data:building_id to prevent duplicate building_name*)
            
                match Osm_xml.Types.find_tag record "highway" with
                | Some _-> 
                    if String.is_prefix id ~prefix:"n" (* highway:motorway_junction that is an intersection*)
                        then (id_seen, street_name_mp, Set.add in_out_intersections id)
                    else
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
                            id_seen, 
                            Map.update street_name_mp street_name ~f:(function
                            | None -> ([id],String.Map.empty)
                            | Some (street_ids,building_mp) -> (id::street_ids,building_mp)
                            ), 
                            in_out_intersections
                        )
                | None ->
                    begin match Osm_xml.Types.find_tag record "addr:street" with
                    | Some street_name ->
                        let building_name = 
                            begin match Osm_xml.Types.find_tag record "name" with
                            | Some name -> name
                            | None ->
                                begin match Osm_xml.Types.find_tag record "addr:housenumber" with
                                | Some house_number -> house_number^" "^street_name
                                | None -> raise (TagNotFound ("name and addr:housenumber",id))
                                end
                            end in
                        (
                            Set.add id_seen id,
                            Map.update street_name_mp street_name ~f:(function
                            | None -> ([],String.Map.singleton building_name id)
                            | Some (street_ids, buildings_map) -> (street_ids, Map.set buildings_map ~key:building_name ~data:id))
                            , in_out_intersections
                        )
                    | None -> (* node that is an intersection between streets inside and outside area*)
                        (id_seen, street_name_mp, Set.add in_out_intersections id)
                    end)