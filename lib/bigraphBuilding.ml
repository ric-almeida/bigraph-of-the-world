open Bigraph
open Core

exception TagNotFound of string * string

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
let build_place_graph (root_level : string) (root_id : string) (root_name : string) = 
    let root_string = root_level^"-"^root_id^"-"^root_name in
    let boundary_to_parent =  Hierarchy.boundary_to_parent root_level root_id root_name in
    let parent_to_boundary = Hierarchy.invert_map_list boundary_to_parent in
    let building_to_parent = Hierarchy.building_to_parent parent_to_boundary root_string in
    let parent_to_building = Hierarchy.invert_map_set building_to_parent in
    let rec helper root_string = 
        let ion = Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [], 0)) in
        let id = Big.atom Link.Face.empty (Ctrl.C ("ID", [S root_string], 0)) in
        let site = Big.split 1 in
        let child_boundary_graphs = 
        match Map.find parent_to_boundary root_string with
        | Some children -> 
            List.map children ~f:(fun child -> helper child)
        | None -> [] in
        let child_buildings = 
        match Map.find parent_to_building root_string with
        | Some l -> l
        | None -> String.Set.empty in
        Big.nest 
            ion 
            (divide_and_par (Map.fold (Hierarchy.street_to_buildings child_buildings root_string) ~init:(id::site::child_boundary_graphs) ~f:(fun ~key:street ~data:buildings street_list-> 
            let street_id = Big.atom Link.Face.empty (Ctrl.C ("ID", [S street], 0)) in
            (Big.nest 
                (Big.ion (Link.parse_face []) (Ctrl.C ("Street", [], 0))) 
                (divide_and_par (Map.fold buildings ~init:[street_id;site] ~f:(fun ~key:building_name ~data:building_id building_list ->
                    let building_id = Big.atom Link.Face.empty (Ctrl.C ("ID", [S (building_id^"-"^building_name)], 0)) in
                    (Big.nest
                        (Big.ion (Link.parse_face []) (Ctrl.C ("Building", [], 0)))
                        (Big.par building_id site))::building_list ))))::street_list))) in
    helper root_string

let add_agent_to_bigraph (agent:Big.t) (bigraph:Big.t) (position:int) = 
    let ord = Big.ord_of_inter (Big.inner bigraph) in
    if position >= ord then
        raise (Invalid_argument ("position "^(string_of_int position)^" not within 0-"^(string_of_int (ord-1))))
    else 
        Big.comp bigraph (Big.ppar_of_list [Big.id (Big.Inter (position, Link.Face.empty)); Big.par (Big.split 1) agent; Big.id (Big.Inter (ord-position-1, Link.Face.empty))])


module MSSolver = Solver.Make_SAT(Solver.MS)
module BRS =  Brs.Make(MSSolver)

let add_agent_to_bigraph_rewrite (agent:Big.t) (bigraph:Big.t) (parent_id:Big.t) = 
    let lhs = parent_id in 
    let rhs = Big.par parent_id agent in 
    let react_add_agent = BRS.parse_react_unsafe ~name:"Add agent"  ~lhs:lhs ~rhs:rhs () None in
    match BRS.step bigraph [react_add_agent] with
    | ((x,_,_)::_,1) ->  x
    | (_,n) -> raise (Not_found_s (Sexplib0.Sexp.message ("Number of possible states: "^(string_of_int n)) []))

let agent = Big.ion (Link.parse_face ["connection"]) (Ctrl.C ("Agent", [], 1))
let boundary = Big.ion (Link.Face.empty) (Ctrl.C ("Boundary", [], 0))
let street = Big.ion (Link.Face.empty) (Ctrl.C ("Street", [], 0))
let building = Big.ion (Link.Face.empty) (Ctrl.C ("Building", [], 0))
let site = Big.split 1

let react_up_boundary = 
    let lhs = 
        Big.nest 
            boundary
            (Big.par site agent) in
    let rhs = Big.par boundary agent in
    BRS.parse_react_unsafe ~name:"Move up boundary"  ~lhs:lhs ~rhs:rhs () None

let react_down_boundary = 
    let lhs = 
        Big.par boundary agent in
    let rhs = 
        Big.nest 
            boundary
            (Big.par site agent) in
    BRS.parse_react_unsafe ~name:"Move down boundary"  ~lhs:lhs ~rhs:rhs () None

let react_up_street =
    let lhs = 
        Big.nest 
            street
            (Big.par site agent) in
    let rhs = Big.par street agent in
    BRS.parse_react_unsafe ~name:"Move up street"  ~lhs:lhs ~rhs:rhs () None

let react_down_street =
    let lhs = Big.par street agent in
    let rhs = 
        Big.nest 
            street
            (Big.par site agent) in
    BRS.parse_react_unsafe ~name:"Move down street"  ~lhs:lhs ~rhs:rhs () None

let react_up_building =
    let lhs = 
        Big.nest 
            building
            (Big.par site agent) in
    let rhs = Big.par building agent in
    BRS.parse_react_unsafe ~name:"Move up building"  ~lhs:lhs ~rhs:rhs () None

let react_down_building =
    let lhs = Big.par building agent in
    let rhs = 
        Big.nest 
            building
            (Big.par site agent) in
    BRS.parse_react_unsafe ~name:"Move down building"  ~lhs:lhs ~rhs:rhs () None

let react_connect_nearby_agents = 
    let disconnected_agent = Big.close (Link.parse_face ["connection"]) agent in
    let lhs = Big.par disconnected_agent disconnected_agent in
    let rhs = 
        Big.close 
            (Link.parse_face ["connection"])
            (Big.par agent agent) in
    BRS.parse_react_unsafe ~name:"Connect nearby agents"  ~lhs:lhs ~rhs:rhs () None

let disconnect_agents = 
    let disconnected_agent = Big.close (Link.parse_face ["connection"]) agent in
    let lhs = 
        Big.close 
            (Link.parse_face ["connection"])
            (Big.ppar agent agent) in
    let rhs = Big.ppar disconnected_agent disconnected_agent in
    BRS.parse_react_unsafe ~name:"Disconnect agents"  ~lhs:lhs ~rhs:rhs () None

let react_rules = 
    [react_up_building; react_up_street; react_up_boundary; 
    react_down_boundary; react_down_street; react_down_building; 
    react_connect_nearby_agents; disconnect_agents]
