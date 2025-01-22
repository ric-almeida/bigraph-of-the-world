open Bigraph
open Core

module StringMap = Map.Make(String)

(** example bigraph*)
let a0 =
  Big.nest
    (Big.ion (Link.parse_face ["a"]) (Ctrl.C ("A", [S "hello"], 1)))
    (Big.nest
 (Big.ion (Link.Face.empty) (Ctrl.C ("Snd", [], 0)))
 (Big.par
    (Big.nest
       (Big.ion (Link.parse_face ["a"; "v_a"]) (Ctrl.C ("M", [], 2)))
       Big.one)
    (Big.nest
       (Big.ion (Link.Face.empty) (Ctrl.C ("Ready", [], 0)))
       (Big.nest
    (Big.ion (Link.Face.empty) (Ctrl.C ("Fun", [], 0)))
    Big.one))))

let test = 
   Big.nest 
      (Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [S "8-295349-Fenland"], 0))) 
      (Big.par (Big.split 1) (Big.nest 
         (Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [S "10-1609095-Whittlesey"], 0))) 
         (Big.par (Big.split 1) (Big.nest 
            (Big.ion (Link.parse_face []) (Ctrl.C ("Street", [S "Church Street"], 0))) 
            (Big.par (Big.split 1) (Big.ion (Link.parse_face []) (Ctrl.C ("Building", [S "685011752-Whittlesey Salvation Army"], 0))))))));;

exception TagNotFound of string * string

(** given boundary_level^boundary_id^boundary_name, return a map of k:street v:building list found in boundary*)
let street_to_buildings boundary = 
   let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/buildings/"^boundary^".osm") in
   let relations = Map.to_alist osm_record.relations in
   let relation_id_tags = List.map relations ~f:(fun (Osm_xml.Types.OSMId id, Osm_xml.Types.OSMRelation relation_record) -> (id, relation_record.tags)) in 
   let ways = Map.to_alist osm_record.ways in
   let ways_id_tags = List.map ways ~f:(fun (Osm_xml.Types.OSMId id, Osm_xml.Types.OSMWay way_record) -> (id,way_record.tags)) in 
   let nodes = Map.to_alist osm_record.nodes in
   let nodes_id_tags = List.map nodes ~f:(fun (Osm_xml.Types.OSMId id, Osm_xml.Types.OSMNode node_record) -> (id, node_record.tags)) in 
   let l = List.concat [relation_id_tags ;ways_id_tags; nodes_id_tags] in
   List.fold l ~init: StringMap.empty ~f: (fun acc (building_id, building_record) -> 
      match Osm_xml.Types.find_tag building_record "name" with
      | None -> raise (TagNotFound ("name",building_id))
      | Some building_name ->
         begin match Osm_xml.Types.find_tag building_record "addr:street" with
         | None -> raise (TagNotFound ("addr:street",building_id))
         | Some street -> 
            begin StringMap.update acc street ~f:(function
            | None -> [building_id^"-"^building_name]
            | Some values -> ((building_id^"-"^building_name) :: values))
            end
         end
      )

(** given a parent to child string-to-string map and a string root, builds the bigraph with that root*)
let rec build_place_graph parent_to_children root = 
   let ion = (Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [S root], 0))) in
   match Map.find parent_to_children root with
   | Some l -> 
      Big.nest 
         ion 
         (Big.par_of_list ((Big.split 1)::(List.map l ~f:(fun s -> build_place_graph parent_to_children s))))
   | None -> 
      Big.nest 
         ion 
         (Big.par_of_list ((Big.split 1)::(List.map (Map.to_alist (street_to_buildings root)) ~f:(fun (street, buildings) -> 
            Big.nest 
               (Big.ion (Link.parse_face []) (Ctrl.C ("Street", [S street], 0))) 
               (Big.par_of_list ((Big.split 1)::(List.map buildings ~f:(fun building -> 
                  Big.ion (Link.parse_face []) (Ctrl.C ("Building", [S building], 0))))))))))

let add_agent_to_bigraph (agent:Big.t) (bigraph:Big.t) (position:int) = 
   let rec add_copies_to_list n copy l = 
      if n > 0 then add_copies_to_list (n-1) copy (copy::l) else l in
   let ord = Big.ord_of_inter (Big.inner bigraph) in
   if position >= ord then
      raise (Invalid_argument ("position "^(string_of_int position)^" not within 0-"^(string_of_int (ord-1))))
   else 
      let id = Big.split 1 in
      let agent_and_after = (Big.par id agent)::(add_copies_to_list (ord-position-1) id []) in
      Big.comp bigraph (Big.ppar_of_list (add_copies_to_list position id agent_and_after))
