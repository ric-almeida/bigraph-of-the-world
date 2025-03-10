(* dune exec bigraph_of_the_world 10 1609095 Whittlesey 92*)
(* dune exec bigraph_of_the_world 8 295349 Fenland 2928*)
(* dune exec bigraph_of_the_world 8 295352 "East Cambridgeshire" 9728*)
(* dune exec bigraph_of_the_world 8 295351 Huntingdonshire 17008*)
(* dune exec bigraph_of_the_world 8 295353 "South Cambridgeshire" 63280*)
(* dune exec bigraph_of_the_world 8 295355 Cambridge 77164*)
(* dune exec bigraph_of_the_world 6 180837 Cambridgeshire 170110 *)

let main (root_level : string) (root_id : string) (root_name : string) write_dot=
    let root_string = root_level^"-"^root_id^"-"^root_name in
    let _ = if not (Lwt_main.run (Lwt_unix.file_exists ("data/boundaries/"^root_string^".osm"))) then 
        (Bigraph_of_the_world.Overpass.query_all_children root_level root_id root_name) in
    let b = Bigraph_of_the_world.BigraphBuilding.build_place_graph root_level root_id root_name in
    let _ = print_endline ("Number of nodes: "^(string_of_int (Bigraph.Nodes.size b.n))) in
    (* let found = Bigraph.Nodes.find_all (Bigraph.Ctrl.C ("ID", [S query_string],0)) b.n in *)
    (* print_endline (Core.List.to_string ~f:string_of_int (Bigraph.IntSet.elements found)) *)
    let _ = if not (Lwt_main.run (Lwt_unix.file_exists "output/")) then Core_unix.mkdir_p ~perm:0o755 "output/" in
    let _ = Lwt_main.run (Bigraph_of_the_world__Overpass.write_to_file (Yojson.Safe.to_string (Bigraph.Big.yojson_of_t b)) ("output/"^root_string^".json")) in
    let _ = print_endline ("Bigraph saved to output/"^root_string^".json") in
    let _ = 
        if write_dot then 
        let _ = Lwt_main.run (Bigraph_of_the_world__Overpass.write_to_file (Bigraph.Big.to_dot b root_string ) ("output/"^root_string^".dot")) in
        print_endline ("Bigraph saved to output/"^root_string^".dot") in
    (* let disconnected_agent = 
        (Big.close
        (Link.parse_face ["connection"])
        (Big.nest
            (Big.ion (Link.parse_face ["connection"]) (Ctrl.C ("Agent", [],0)))
            (Big.atom (Link.Face.empty) (Ctrl.C ("ID", [S "agent 1"],0))))) in
    let with_agent = Bigraph_of_the_world.BigraphBuilding.add_agent_to_bigraph (Big.par disconnected_agent disconnected_agent) b 0 in *)
    (* let with_agent = Bigraph_of_the_world.BigraphBuilding.add_agent_to_bigraph_rewrite (Big.par disconnected_agent disconnected_agent) b (Big.atom Link.Face.empty (Ctrl.C ("ID", [S query_string], 0))) in *)
    (* print_endline ("Number of nodes: "^(string_of_int (Bigraph.Nodes.size with_agent.n))) *)
    (* let module MSSolver = Solver.Make_SAT(Solver.MS) in *)
    (* print_int (List.length (MSSolver.occurrences ~target:b ~pattern:(Big.atom Link.Face.empty (Ctrl.C ("ID", [S query_string], 0))))) *)
    (* let (_,possibilities) = Bigraph_of_the_world.BigraphBuilding.BRS.step with_agent [Bigraph_of_the_world.BigraphBuilding.react_connect_nearby_agents] in *)
    (* print_endline (string_of_int possibilities) *)
    ()

let command =
    Core.Command.basic
        ~summary:"Build bigraph of boundary"
        ~readme:(fun () -> "Given a query boundary, build a Bigraph with that boundary as its root. This Bigraph will contain the hierarchy of administrative boundaries, streets and buildings contained within the query boundary. The tool works for any relation on OSM with \"admin_level\", \"id\", and \"name\" tags.\nExample: ./botw.exe 8 1481934 Dover")
        (let%map_open.Core.Command root_level = anon ("boundary_admin_level" %: string)
        and root_relation = anon ("boundary_relation_id" %: string) 
        and root_name = anon ("boundary_name" %: string) 
        and write_dot = flag "-write-dot" no_arg ~doc:"export the Bigraph to a dot-file" in
        fun () -> main root_level root_relation root_name write_dot)

let () = 
    (* Memtrace.trace_if_requested (); *)
    Command_unix.run command
