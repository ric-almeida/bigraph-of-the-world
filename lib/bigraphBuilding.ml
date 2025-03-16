open Bigraph
open Core

exception TagNotFound of string * string

(* let rec divide_and_par x = 
  let rec split x l r = match x with
  | [] -> (l,r)
  | x::xs -> split xs r (x::l) in 
  match x with
  | [] -> Big.id_eps
  | x::[] -> x
  | _ -> let (l,r) = split x [] [] 
      in (Big.par (divide_and_par l) (divide_and_par r)) *)

let rec par_of_list_top_down bs = 
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
    Big.par (par_of_list_top_down l) (par_of_list_top_down r)

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
            let rec helper buildingid_seen boundary = 
                let ion = Big.ion (Link.parse_face []) Ctrl.{ s = "Boundary"; p = []; i = 0 } in
                let id = Big.atom Link.Face.empty Ctrl.{ s = "ID"; p = [S boundary]; i = 0 } in
                (* let site = Big.split 1 in *)
                let site = Big.id_eps in
                let (buildingid_seen, child_boundary_graphs) = 
                    match Map.find boundary_to_children boundary with
                    | Some children -> 
                        List.fold children 
                            ~init:(buildingid_seen, []) 
                            ~f:(fun (buildingid_seen,child_boundary_graphs) child -> 
                                let (buildingid_seen, child_place_graph) = helper buildingid_seen child in
                                (buildingid_seen, child_place_graph::child_boundary_graphs))
                    | None -> (buildingid_seen,[]) in
                let (buildingid_seen, streets) = Hierarchy.street_to_building buildingid_seen boundary in
                let place_graph =
                    Big.nest 
                        ion 
                        (par_of_list_top_down 
                            (Map.fold streets
                                ~init:(id::site::child_boundary_graphs) 
                                ~f:(fun ~key:street ~data:buildings boundary_children_bigraphs-> 
                                    let street_id = Big.atom Link.Face.empty Ctrl.{ s = "ID"; p = [S street]; i = 0 } in
                                    (Big.nest 
                                        (Big.ion (Link.parse_face []) Ctrl.{ s = "Street"; p = []; i = 0 }) 
                                        (par_of_list_top_down 
                                            (Map.fold buildings 
                                                ~init:[street_id; site] 
                                                ~f:(fun ~key:building_name ~data:building_id street_children_bigraphs ->
                                                    let building_id = Big.atom Link.Face.empty Ctrl.{ s = "ID"; p = [S (building_id^"-"^building_name)]; i = 0 } in
                                                    (Big.nest
                                                        (Big.ion (Link.parse_face []) Ctrl.{ s = "Building"; p = []; i = 0 })
                                                        (Big.par building_id site))
                                                    ::street_children_bigraphs ))))
                                    ::boundary_children_bigraphs))) in
                let _ = report_progress 1 in
                (buildingid_seen,place_graph) in
            let (_, place_graph) = helper String.Set.empty root_string in
            place_graph)

(* let add_agent_to_bigraph (agent:Big.t) (bigraph:Big.t) (position:int) = 
    (* let site = Big.split 1 in *)
    let site = Big.id_eps in
    let ord = Big.ord_of_inter (Big.inner bigraph) in
    if position >= ord then
        raise (Invalid_argument ("position "^(string_of_int position)^" not within 0-"^(string_of_int (ord-1))))
    else 
        Big.comp bigraph (Big.ppar_of_list [Big.id (Big.Inter (position, Link.Face.empty)); Big.par site agent; Big.id (Big.Inter (ord-position-1, Link.Face.empty))]) *)


module MSSolver = Solver.Make_SAT(Solver.MS)
module BRS =  Brs.Make(MSSolver)

let add_agent_to_bigraph_react (agent:Big.t) (bigraph:Big.t) (parent_id:Big.t) = 
    let lhs = parent_id in 
    let rhs = Big.par parent_id agent in 
    let react_add_agent = BRS.parse_react_unsafe ~name:"Add agent"  ~lhs:lhs ~rhs:rhs () None in
    match BRS.step bigraph [react_add_agent] with
    | ((x,_,_)::_,1) ->  x
    | (_,n) -> raise (Not_found_s (Sexplib0.Sexp.message ("Number of possible states: "^(string_of_int n)) []))

let add_agent_to_bigraph_rewrite (b:Big.t) (parent_id:string) (agent:Big.t) = 
    let parent_id_node = Ctrl.{ s = "ID"; p = [S parent_id]; i = 0 } in
    let (parent_index,_) = 
        let found = Nodes.find_all parent_id_node b.n in
        match Iso.to_list (IntSet.IntSet.fix found) with
        | [x] -> x
        | l -> raise (Not_found_s (Sexplib0.Sexp.message ("Number of possible states: "^(string_of_int (List.length l))) []))
    in
    let parent_id_bigraph = Big.atom (Link.Face.empty) parent_id_node in
    Big.rewrite (Iso.add 0 parent_index Iso.empty,Iso.empty,Fun.empty) ~s:b ~r0:parent_id_bigraph ~r1:(Big.par agent parent_id_bigraph) None;;

let agent = Big.ion (Link.parse_face ["connection"]) Ctrl.{ s = "Agent"; p = []; i = 1 }
let boundary = Big.ion (Link.Face.empty) Ctrl.{ s = "Boundary"; p = []; i = 0 }
let street = Big.ion (Link.Face.empty) Ctrl.{ s = "Street"; p = []; i = 0 }
let building = Big.ion (Link.Face.empty) Ctrl.{ s = "Building"; p = []; i = 0 }
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
