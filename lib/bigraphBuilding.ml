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

let add_sites_to_right_then_nest f g =
    let diff = Big.ord_of_inter (Big.inner f) - Big.ord_of_inter (Big.outer g) in
    let site = Big.split 1 in
    let rec add_sites l = function
    | 0 -> l
    | n -> add_sites (site::l) (n-1) in
    if diff>=0 
        then    Big.nest f (of_list_top_down Big.ppar (g::(add_sites [] diff)))
    else    Big.nest (of_list_top_down Big.ppar (f::(add_sites [] (-diff)))) g
    

(** given a parent to child string-to-string map and a string root, builds the bigraph with that root*)
let build_place_graph (root_level : string) (root_id : string) (root_name : string) = 
    let root_string = root_level^"-"^root_id^"-"^root_name in
    let _ = if not (Lwt_main.run (Lwt_unix.file_exists ("data/"^root_string^".osm"))) then 
        (Overpass.query_all_children root_level root_id root_name) in
    let boundary_to_parent =  Hierarchy.boundary_to_parent root_level root_id root_name in
    let boundary_to_children = Hierarchy.invert_map_list boundary_to_parent in
    let bar =
        let total = (Map.length boundary_to_parent) in
        let open Progress.Line in
        list [ const "Building bigraph"; elapsed (); bar total; percentage_of  total ] in
    Progress.with_reporter
        bar
        (fun report_progress ->
            let site_no = ref 2 in (* site 0 is for id sibling, site 1 is for boundary sibling*)
            let junction_hash = String.Table.create () in
            let rec helper boundary id_seen shared_intersections comp_list= 
                let (_,boundary_id_name) = String.lsplit2_exn boundary ~on:'-' in
                let (_,boundary_name) = String.lsplit2_exn boundary_id_name ~on:'-' in
                let site = Big.split 1 in
                let (id_seen, shared_intersections, boundary_children_bigraphs) = 
                    let children_boundaries = 
                        match Map.find boundary_to_children boundary with
                        | Some children -> children
                        | None -> [] in
                    List.fold children_boundaries 
                        ~init:(id_seen, shared_intersections, (Big.ppar site Big.one)::comp_list) (* site for id and one to close sibling boundaries *)
                        ~f:(fun (id_seen, shared_intersections, child_boundary_graphs) child -> 
                            let (id_seen, _, shared_intersections, child_boundary_graphs) =
                                helper child id_seen shared_intersections child_boundary_graphs in
                            (id_seen, shared_intersections, child_boundary_graphs)) in
                let (new_id_seen, street_name_to_street_ids_and_buildings, in_out_intersections) = Hierarchy.idseen_streetnamemap_inoutintersections id_seen boundary in
                let shared_intersections = Set.union in_out_intersections shared_intersections in
                let streetid_to_junctions = Hierarchy.streetid_to_junctions boundary id_seen shared_intersections in
                let place_graph =
                    (Big.close
                        (Link.parse_face ["id"])
                        (Big.ppar
                            (Big.par
                                site (* sibiling id*)
                                (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S boundary_name]; i = 1 })
                            )
                            (Big.par
                                site (* sibiling boundary*)
                                (Big.ion (Link.parse_face ["id"]) Ctrl.{ s = "Boundary"; p = [S boundary_name]; i = 1 }) (* site for children*)
                            )
                        )
                    ):: (* nest *)
                    (Map.fold street_name_to_street_ids_and_buildings
                        ~init:((Big.placing [[0];[2];[1]] 3 Link.Face.empty)::boundary_children_bigraphs) (* boundaries only have 2 regions, streets have 3 sites. add site to right then move to middle*)
                        ~f:(fun ~key:street_name ~data:(street_ids,buildings) boundary_children_bigraphs->
                            let junctions =
                                List.fold street_ids ~init:[] ~f:(fun junctions street_id ->
                                    match Map.find streetid_to_junctions street_id with
                                    | None -> junctions
                                    | Some js -> junctions@js) in
                            (Big.close
                                (Link.parse_face ["id"])
                                (Big.ppar
                                    (Big.par
                                        site (* sibiling id*)
                                        (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S street_name]; i = 1 })
                                    )
                                    (Big.ppar
                                        site (*uncle boundary*)
                                        (Big.par
                                            site (* sibiling street/boundary*)
                                            (Big.ion (Link.parse_face ["id"]) Ctrl.{ s = "Street"; p = [S street_name]; i = 1 }) (*site for children*)
                                        )
                                    )
                                ) 
                            )::(* nest*)
                            (let junction_bigs = 
                                List.fold junctions 
                                    ~init:((Big.ppar site (Big.ppar site (Big.ppar site Big.one)))::boundary_children_bigraphs) (* close open-ended buildings sibling*)
                                    ~f:(fun junction_bigs junction->
                                        (Big.ppar
                                            site (* no id*)
                                            (Big.ppar
                                                (Big.ppar
                                                    site (*grand-uncle boundary*)
                                                    site (*uncle street/boundary*)
                                                )
                                                (Big.par
                                                    site (* sibiling junction*)
                                                    (* (let _ = 
                                                        Hashtbl.update junction_hash junction ~f:(function
                                                        | None -> [!site_no]
                                                        | Some l -> (!site_no)::l
                                                        ) in
                                                        let _ = site_no := !site_no +1 in
                                                    site 
                                                    ) *)
                                                    (Big.atom (Link.parse_face [junction]) Ctrl.{ s = "Junction"; p = []; i = 1 })
                                                    (* Big.id_eps *)
                                                )
                                            )
                                        )::
                                        junction_bigs)
                                        in
                            Map.fold buildings 
                                ~init:junction_bigs
                                ~f:(fun ~key:building_name ~data:_ street_children_bigraphs ->
                                    (Big.close
                                        (Link.parse_face ["id"])
                                        (Big.ppar
                                            (Big.par 
                                                site (* sibling id*)
                                                (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S (building_name)]; i = 1 })
                                            )
                                            (Big.ppar
                                                (Big.ppar
                                                    site (*grand-uncle boundary*)
                                                    site (*uncle street/boundary*)
                                                )
                                                (Big.par
                                                    site (* sibiling building*)
                                                    (Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "Building"; p = [S (building_name)]; i = 1 })
                                                )
                                            )
                                        )
                                    )::
                                    street_children_bigraphs    
                                )
                            )
                        )
                    )
                    in
                let _ = report_progress 1 in
                (new_id_seen, in_out_intersections, shared_intersections, place_graph) in
            let (_, in_out_intersections, _, place_graph) = helper root_string String.Set.empty String.Set.empty [Big.ppar Big.one Big.one] in
            let g = of_list_top_down (add_sites_to_right_then_nest) place_graph in g
            (* let outer_names = Big.face_of_inter (Big.outer g) in
            let names_to_close = Link.Face.diff outer_names (Link.parse_face (Set.to_list in_out_intersections)) in
            Big.close (names_to_close) g *)
            (* let junction_lists = [0]::[1]::(Hashtbl.fold junction_hash ~init:[] ~f:(fun ~key:_ ~data:l acc->l::acc)) in
            let shared_junction_placing = Big.placing junction_lists (!site_no) Link.Face.empty in
            let junction = Big.atom Link.Face.empty Ctrl.{ s = "Junction"; p = []; i = 0 } in
            let rec add_junction l = function
            | 0 -> l
            | n -> add_junction (junction::l) (n-1) in
            let junction_big_list = (add_junction [] (Hashtbl.length junction_hash)) in
            let f = of_list_top_down (Big.ppar) (Big.one::Big.one::junction_big_list) in *)
            (* Big.share f shared_junction_placing g *)
            (* Big.comp g (Big.comp shared_junction_placing f) *)
            (* of_list_top_down (add_sites_to_right_then_comp) (place_graph@[shared_junction_placing;f]) *)
        )

module MSSolver = Solver.Make_SAT(Solver.MS)
module BRS =  Brs.Make(MSSolver)

let add_agent_to_building_react ~bigraph ~agent_id  ~building_id= 
    let parent = Big.ion (Link.parse_face ["id"]) Ctrl.{ s = "Building"; p = []; i = 1 } in
    let parent_id = Big.atom (Link.parse_face ["id"]) Ctrl.{ s = "ID"; p = [S building_id]; i = 1 } in
    let lhs = 
        Big.close
            (Link.parse_face ["id"])
            (Big.ppar parent parent_id) in 
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
                    parent
                    (Big.par parent_id site)
                ) 
            )
            (Big.close
                (Link.parse_face ["id"])
                (Big.ppar (Big.par child site) child_id)
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
