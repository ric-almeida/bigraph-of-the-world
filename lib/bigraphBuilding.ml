open Bigraph
open Core

module StringMap = Map.Make(String)

(** example bigraph*)
let test = 
   Big.nest 
      (Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0))) 
      (Big.par_of_list [Big.atom Link.Face.empty (Ctrl.C ("ID", [S "8-295349-Fenland"], 0));(Big.split 1) ;(Big.nest 
         (Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0))) 
         (Big.par_of_list [Big.atom Link.Face.empty (Ctrl.C ("ID", [S "10-1609095-Whittlesey"],0)); Big.split 1 ;(Big.nest 
            (Big.ion (Link.parse_face []) (Ctrl.C ("Street", [], 0))) 
            (Big.par_of_list [Big.atom Link.Face.empty (Ctrl.C ("ID", [S "Church Street"],0));Big.split 1; Big.nest
               (Big.ion (Link.parse_face []) (Ctrl.C ("Building", [], 0)))
               (Big.par 
                  (Big.split 1) 
                  (Big.atom Link.Face.empty (Ctrl.C ("ID", [S "685011752-Whittlesey Salvation Army"], 0)))
               )]
            ))]
         ))]
      );;

exception TagNotFound of string * string
module StringSet = Set.Make(String)

(** given boundary_level^boundary_id^boundary_name, return a map of k:street v:building list found in boundary*)
let street_to_buildings boundary = 
   let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file ("data/buildings/"^boundary^".osm") in
   let relation_id_tags = Map.fold osm_record.relations ~init:[] ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMRelation relation_record) l -> (id, relation_record.tags)::l) in 
   let relation_ways_id_tags = Map.fold osm_record.ways ~init:relation_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMWay way_record) l -> (id,way_record.tags)::l) in 
   let relation_ways_nodes_id_tags = Map.fold osm_record.nodes ~init:relation_ways_id_tags ~f:(fun ~key:(Osm_xml.Types.OSMId id) ~data:(Osm_xml.Types.OSMNode node_record) l -> (id, node_record.tags)::l) in 
   List.fold relation_ways_nodes_id_tags ~init:StringMap.empty ~f:(fun streets_map (building_id, building_record) -> 
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

let rec divide_and_par x = 
   let rec split x l r = match x with
   | [] -> (l,r)
   | x::xs -> split xs r (x::l) in 
   match x with
   | [] -> Big.id_eps
   | x::[] -> x
   | _ -> let (l,r) = split x [] [] 
      in (Big.par (divide_and_par l) (divide_and_par r))

(** given a parent to child string-to-string map and a string root, builds the bigraph with that root*)
let rec build_place_graph parent_to_children root = 
   let ion = Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0)) in
   let id = Big.atom Link.Face.empty (Ctrl.C ("ID", [S root], 0)) in
   let site = Big.split 1 in
   match Map.find parent_to_children root with
   | Some children -> 
      Big.nest 
         ion 
         (divide_and_par (id::site::(List.map children ~f:(fun child -> build_place_graph parent_to_children child))))
   | None -> 
      Big.nest 
         ion 
         (divide_and_par (Map.fold (street_to_buildings root) ~init:[id;site] ~f:(fun ~key:street ~data:buildings street_list-> 
            let street_id = Big.atom Link.Face.empty (Ctrl.C ("ID", [S street], 0)) in
            (Big.nest 
               (Big.ion (Link.parse_face []) (Ctrl.C ("Street", [], 0))) 
               (divide_and_par (Map.fold buildings ~init:[street_id;site] ~f:(fun ~key:building_name ~data:building_id building_list ->
                  let building_id = Big.atom Link.Face.empty (Ctrl.C ("ID", [S (building_id^"-"^building_name)], 0)) in
                  (Big.nest
                     (Big.ion (Link.parse_face []) (Ctrl.C ("Building", [], 0)))
                     (Big.par building_id site))::building_list ))))::street_list)))

let add_agent_to_bigraph (agent:Big.t) (bigraph:Big.t) (position:int) = 
   let ord = Big.ord_of_inter (Big.inner bigraph) in
   if position >= ord then
      raise (Invalid_argument ("position "^(string_of_int position)^" not within 0-"^(string_of_int (ord-1))))
   else 
      Big.comp bigraph (Big.ppar_of_list [Big.id (Big.Inter (position, Link.Face.empty)); Big.par (Big.split 1) agent; Big.id (Big.Inter (ord-position-1, Link.Face.empty))])


module MSSolver = Solver.Make_SAT(Solver.MS)
module BRS =  Brs.Make(MSSolver)

let add_agent_to_bigraph_rewrite (agent:Big.t) (bigraph:Big.t) (parent_id:Big.t) = 
   let occ_list = MSSolver.occurrences ~target:bigraph ~pattern:parent_id in 
   match occ_list with
   | [o] -> Big.rewrite (o.nodes, o.edges, o.hyper_edges) ~s:bigraph ~r0:parent_id ~r1:(Big.par parent_id agent)
   | _ -> raise (Not_found_s (Sexplib0.Sexp.message "not found" []))

let react_up_boundary_to_boundary = 
   let boundary = Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0)) in
   let agent = Big.ion (Link.parse_face []) (Ctrl.C ("Agent", [], 0)) in
   let site = Big.split 1 in
   let lhs = 
      Big.nest 
         boundary
         (Big.par site (Big.nest 
            boundary
            (Big.par site agent))) in
   let rhs = 
      Big.nest
         boundary
         (Big.par_of_list [site; Big.nest boundary site; agent ] ) in
   BRS.parse_react_unsafe ~name:"Move up boundary to boundary"  ~lhs:lhs ~rhs:rhs () None

let react_down_boundary_to_boundary = 
   let boundary = Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0)) in
   let agent = Big.ion (Link.parse_face []) (Ctrl.C ("Agent", [], 0)) in
   let site = Big.split 1 in
   let lhs = 
      Big.nest
         boundary
         (Big.par_of_list [site; Big.nest boundary site; agent ] ) in
   let rhs = 
      Big.nest 
         boundary
         (Big.par site (Big.nest 
            boundary
            (Big.par site agent))) in
   BRS.parse_react_unsafe ~name:"Move down boundary to boundary"  ~lhs:lhs ~rhs:rhs () None

let react_up_street_to_boundary =
   let boundary = Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0)) in
   let street = Big.ion (Link.parse_face []) (Ctrl.C ("Street", [], 0)) in
   let agent = Big.ion (Link.parse_face []) (Ctrl.C ("Agent", [], 0)) in
   let site = Big.split 1 in
   let lhs = 
      Big.nest 
         boundary
         (Big.par site (Big.nest 
            street
            (Big.par site agent))) in
   let rhs = 
      Big.nest
         boundary
         (Big.par_of_list [site; Big.nest street site; agent] ) in
   BRS.parse_react_unsafe ~name:"Move up street to boundary"  ~lhs:lhs ~rhs:rhs () None

let react_down_boundary_to_street =
   let boundary = Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0)) in
   let street = Big.ion (Link.parse_face []) (Ctrl.C ("Street", [], 0)) in
   let agent = Big.ion (Link.parse_face []) (Ctrl.C ("Agent", [], 0)) in
   let site = Big.split 1 in
   let lhs = 
      Big.nest
         boundary
         (Big.par_of_list [site; Big.nest street site; agent ] ) in
   let rhs = 
      Big.nest 
         boundary
         (Big.par site (Big.nest 
            street
            (Big.par site agent))) in
   BRS.parse_react_unsafe ~name:"Move down boundary to street"  ~lhs:lhs ~rhs:rhs () None

let react_up_building_to_street =
   let street = Big.ion (Link.parse_face []) (Ctrl.C ("Street", [], 0)) in
   let building = Big.ion (Link.parse_face []) (Ctrl.C ("Building", [], 0)) in
   let agent = Big.ion (Link.parse_face []) (Ctrl.C ("Agent", [], 0)) in
   let site = Big.split 1 in
   let lhs = 
      Big.nest 
         street
         (Big.par site (Big.nest 
            building
            (Big.par site agent))) in
   let rhs = 
      Big.nest
         street
         (Big.par_of_list [site; Big.nest building site; agent] ) in
   BRS.parse_react_unsafe ~name:"Move up building to street"  ~lhs:lhs ~rhs:rhs () None

let react_down_street_to_building =
   let street = Big.ion (Link.parse_face []) (Ctrl.C ("Street", [], 0)) in
   let building = Big.ion (Link.parse_face []) (Ctrl.C ("Building", [], 0)) in
   let agent = Big.ion (Link.parse_face []) (Ctrl.C ("Agent", [], 0)) in
   let site = Big.split 1 in
   let lhs = 
      Big.nest
         street
         (Big.par_of_list [site; Big.nest building site; agent] ) in
   let rhs = 
      Big.nest 
         street
         (Big.par site (Big.nest 
            building
            (Big.par site agent))) in
   BRS.parse_react_unsafe ~name:"Move down street to building"  ~lhs:lhs ~rhs:rhs () None

let react_rules = [react_up_building_to_street; react_up_street_to_boundary; react_up_boundary_to_boundary; react_down_boundary_to_boundary; react_down_boundary_to_street; react_down_street_to_building]