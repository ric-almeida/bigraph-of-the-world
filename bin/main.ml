(* dune exec bigraph_of_the_world 10 1609095 Whittlesey*)
(* dune exec bigraph_of_the_world 8 295349 Fenland*)
(* dune exec bigraph_of_the_world 8 295355 Cambridge*)
(* dune exec bigraph_of_the_world 8 295351 Huntingdonshire*)
(* dune exec bigraph_of_the_world 8 295352 "East Cambridgeshire"*)
(* dune exec bigraph_of_the_world 8 295353 "South Cambridgeshire"*)
(* dune exec bigraph_of_the_world 6 180837 Cambridgeshire*)

let do_hash admin_level query_relation name =
  let mp =  Bigraph_of_the_world.Hierarchy.build_hierarchy  admin_level query_relation name in
  let parent_child = Bigraph_of_the_world.Hierarchy.invert_map mp in
  let query_string = admin_level^"-"^query_relation^"-"^name in
  let b = Bigraph_of_the_world.BigraphBuilding.build_place_graph parent_child query_string in
  print_endline (Bigraph.Big.string_of_inter (Bigraph.Big.inner b))

let command =
  Core.Command.basic
    ~summary:"Build bigraph of boundary"
    ~readme:(fun () -> "Given a query boundary, build a bigraph with that boundary as root")
    Core.Command.Param.(
      map
        (both
          (both 
            (anon ("boundary_admin_level" %: string))
            (anon ("boundary_relation_id" %: string)))
          (anon ("boundary_name" %:string))
        )
        ~f:(fun ((admin_level,query_relation),name) () -> do_hash admin_level query_relation name))

let () = Core.Command.run ~version:"1.0" ~build_info:"RWO" command