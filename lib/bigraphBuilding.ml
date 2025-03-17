open Bigraph
open Core

exception TagNotFound of string * string

let rec of_list_top_down f bs = 
    let split xs = 
        let rec move s d k =
            match s with 
            | [] -> ([],d)
            | x::xs -> 
                if k>0 then move xs (x::d) (k-1) 
                else (s,d) in
        let k = List.length xs /2 in
        let (r, l_reversed) = move xs [] k in
        (List.rev l_reversed, r) in
    match bs with
    | [] -> Big.id_eps
    | [b] -> b
    | _ -> 
        let (l,r) = split bs in
    f (of_list_top_down f l) (of_list_top_down f r)

(* let par_of_list_bottom_up bs = 
    let rec helper bs acc = match bs,acc with
    | b1::b2::bs, acc -> helper bs ((Big.par b1 b2)::acc)
    | [b], [] -> b
    | [b], acc -> helper (List.rev (b::acc)) []
    | [], [] -> Big.id_eps
    | [], acc -> helper (List.rev acc) [] in
    helper bs [] *)

(** given a parent to child string-to-string map and a string root, builds the bigraph with that root*)
let build_place_graph (root_level : string) (root_id : string) (root_name : string) = 
    let root_string = root_level^"-"^root_id^"-"^root_name in
    let boundary_to_parent =  Hierarchy.boundary_to_parent root_level root_id root_name in
    let boundary_to_children = Hierarchy.invert_map_list boundary_to_parent in
    let bar =
        let total = (Map.length boundary_to_parent) in
        let open Progress.Line in
        list [ const "Building bigraph"; elapsed (); bar total; percentage_of  total ] in
    Progress.with_reporter
        bar
        (fun report_progress ->
            let rec helper buildingid_seen boundary comp_list= 
                let site = Big.split 1 in
                let (buildingid_seen, boundary_children_bigraphs) = 
                    let children_boundaries = 
                        match Map.find boundary_to_children boundary with
                        | Some children -> children
                        | None -> [] in
                    List.fold children_boundaries 
                        ~init:(buildingid_seen, [Big.ppar site Big.one]) 
                        ~f:(fun (buildingid_seen,child_boundary_graphs) child -> 
                            helper buildingid_seen child child_boundary_graphs) in
                let (buildingid_seen, streets) = Hierarchy.street_to_building buildingid_seen boundary in
                let place_graph =
                    (Big.close
                        (Link.parse_face ["id"])
                        (Big.ppar
                            (Big.par
                                (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S boundary]; i = 1 })
                                site (* sibiling id*)
                            )
                            (Big.par
                                (Big.ion (Link.parse_face ["id"]) Ctrl.{ s = "Boundary"; p = []; i = 1 })
                                site (* sibiling boundary*)
                            )
                        )
                    )::
                    (Big.ppar
                        (of_list_top_down (Big.comp) 
                            (Map.fold streets
                                ~init:boundary_children_bigraphs
                                ~f:(fun ~key:street ~data:buildings boundary_children_bigraphs->
                                    (* nest*)
                                    (Big.close
                                        (Link.parse_face ["id"])
                                        (Big.ppar
                                            (Big.par
                                                (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S street]; i = 1 })
                                                site (* sibiling id*)
                                            )
                                            (Big.par
                                                (Big.ion (Link.parse_face ["id"]) Ctrl.{ s = "Street"; p = []; i = 1 })
                                                site (* sibiling street*)
                                            )
                                        ) 
                                    )::
                                    (Big.ppar 
                                        (of_list_top_down (Big.comp) (* par buildings*)
                                            (Map.fold buildings 
                                                ~init:[Big.ppar site Big.one] (* close open-ended buildings sibling*)
                                                ~f:(fun ~key:building_name ~data:building_id street_children_bigraphs ->
                                                    (Big.close
                                                        (Link.parse_face ["id"])
                                                        (Big.ppar
                                                            (Big.par 
                                                                (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S (building_id^"-"^building_name)]; i = 1 })
                                                                site (* sibling id*)
                                                            )
                                                            (Big.par
                                                                (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "Building"; p = []; i = 1 })
                                                                site (* sibiling building*)
                                                            )
                                                        )
                                                    )::
                                                    street_children_bigraphs    
                                                )
                                            )
                                        )
                                        site (* sibling street*)
                                    )::
                                    boundary_children_bigraphs
                                )
                            )
                        )
                        site (* sibling boundary*)
                    )::comp_list in
                let _ = report_progress 1 in
                (buildingid_seen,place_graph) in
            let (_, place_graph) = helper String.Set.empty root_string [Big.ppar Big.one Big.one]in
           of_list_top_down (Big.comp) place_graph
        )

module MSSolver = Solver.Make_SAT(Solver.MS)
module BRS =  Brs.Make(MSSolver)

let add_agent_to_building_react ~bigraph ~agent_id  ~building_id= 
    let parent = Big.ion (Link.parse_face ["id"]) Ctrl.{ s = "Building"; p = []; i = 1 } in
    let parent_id = Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S building_id]; i = 1 } in
    let lhs = 
        Big.close
            (Link.parse_face ["id"])
            (Big.ppar parent_id parent) in 
    let site = Big.split 1 in
    let child = 
        Big.close
            (Link.parse_face ["connection"])
            (Big.atom (Link.parse_face ["id";"connection"]) Ctrl.{ s = "Agent"; p = []; i = 2 }) in
    let child_id = Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S agent_id]; i = 1 } in
    let rhs = 
        Big.nest 
            (Big.close
                (Link.parse_face ["id"])
                (Big.ppar 
                    (Big.par parent_id site)
                    parent) 
            )
            (Big.close
                (Link.parse_face ["id"])
                (Big.ppar child_id (Big.par child site))
            ) in 
    let react_add_agent = BRS.parse_react_unsafe ~name:("Add agent "^agent_id^" to building "^building_id)  ~lhs:lhs ~rhs:rhs () None in
    match BRS.step bigraph [react_add_agent] with
    | ((x,_,_)::_,1) ->  x
    | (_,n) -> raise (Not_found_s (Sexplib0.Sexp.message ("Number of possible states: "^(string_of_int n)) []))

let agent = Big.atom (Link.parse_face ["agent_id";"connection"]) Ctrl.{ s = "Agent"; p = []; i = 2 }
let agent2 = Big.atom (Link.parse_face ["agent_id2";"connection"]) Ctrl.{ s = "Agent"; p = []; i = 2 }
let disconnected_agent = Big.close (Link.parse_face ["connection"]) agent
let disconnected_agent2 = Big.close (Link.parse_face ["connection"]) agent2
let boundary = Big.ion (Link.parse_face ["boundary_id"]) Ctrl.{ s = "Boundary"; p = []; i = 1 }
let street = Big.ion (Link.parse_face ["street_id"]) Ctrl.{ s = "Street"; p = []; i = 1 }
let building = Big.ion (Link.parse_face ["building_id"]) Ctrl.{ s = "Building"; p = []; i = 1 }
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
    let lhs = Big.par disconnected_agent disconnected_agent2 in
    let rhs = 
        Big.close 
            (Link.parse_face ["connection"])
            (Big.par agent agent2) in
    BRS.parse_react_unsafe ~name:"Connect nearby agents"  ~lhs:lhs ~rhs:rhs () None

let react_disconnect_agents = 
    let lhs = 
        Big.close 
            (Link.parse_face ["connection"])
            (Big.ppar agent agent2) in
    let rhs = Big.ppar disconnected_agent disconnected_agent2 in
    BRS.parse_react_unsafe ~name:"Disconnect agents"  ~lhs:lhs ~rhs:rhs () None

let react_rules = 
    [react_up_building; react_up_street; react_up_boundary; 
    react_down_boundary; react_down_street; react_down_building; 
    react_connect_nearby_agents; react_disconnect_agents]
