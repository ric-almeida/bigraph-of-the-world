(* _build/default/bin/botw.exe 10 2604777 Dover Number of nodes: 2184*)
(* _build/default/bin/botw.exe 8 1481934 Dover Number of nodes: 14954*)
(* _build/default/bin/botw.exe 10 1628638 Tilbrook Number of nodes: 35*)
(* _build/default/bin/botw.exe 8 295349 Fenland Number of nodes: 13480*)
(* _build/default/bin/botw.exe 8 295352 "East Cambridgeshire" Number of nodes: 19619*)
(* _build/default/bin/botw.exe 8 295351 Huntingdonshire Number of nodes: 37597*)
(* _build/default/bin/botw.exe 8 295355 Cambridge Number of nodes: 62838*)
(* _build/default/bin/botw.exe 8 295353 "South Cambridgeshire" Number of nodes: 75961*)
(* _build/default/bin/botw.exe 6 180837 Cambridgeshire Number of nodes: 210768 *)

let main (root_level : string) (root_id : string) (root_name : string) write_dot id_in_parameter=
    let root_string = root_level^"-"^root_id^"-"^root_name in
    let b = Bigraph_of_the_world.Bigraph_building.build_place_graph root_level root_id root_name id_in_parameter in
    let _ = print_endline ("Number of nodes: "^(string_of_int (Bigraph.Nodes.size b.n))) in
    (* let b = Bigraph_of_the_world.Bigraph_building.add_agent_to_building_react ~bigraph:b ~agent_id:"Agent A" ~building_name:"Tilbrook Village Hall" in
    let _ = print_endline ("Added agent by reaction. Number of nodes: "^(string_of_int (Bigraph.Nodes.size b.n))) in *)
    let _ = if not (Lwt_main.run (Lwt_unix.file_exists "output/")) then Core_unix.mkdir_p ~perm:0o755 "output/" in
    (* let _ = Lwt_main.run (Bigraph_of_the_world__Overpass.write_to_file (Yojson.Safe.to_string (Bigraph.Big.yojson_of_t b)) ("output/"^root_string^".json")) in
    let _ = print_endline ("Bigraph saved to output/"^root_string^".json") in *)
    let _ = if write_dot then 
    begin
        let _ = if not (Lwt_main.run (Lwt_unix.file_exists "output/renders")) then Core_unix.mkdir_p ~perm:0o755 "output/renders" in
        let _ = Lwt_main.run (Bigraph_of_the_world__Overpass.write_to_file (Bigraph.Big.to_dot b root_string ) ("output/renders/"^root_string^".dot")) in
        print_endline ("Bigraph saved to output/renders/"^root_string^".dot")
    end in
    (* let _ = Lwt_main.run (Bigraph_of_the_world__Overpass.write_to_file (Bigraph.Tikz.big_to_tikz b) ("output/"^root_string^".tikz")) in
    let _ = print_endline ("Bigraph saved to output/"^root_string^".tikz") in *)
    ()

let command =
    Core.Command.basic
        ~summary:"Build bigraph of boundary"
        ~readme:(fun () -> "Given a query boundary, build a Bigraph with that boundary as its root. This Bigraph will contain the hierarchy of administrative boundaries, streets and buildings contained within the query boundary. The tool works for any relation on OSM with \"admin_level\", \"id\", and \"name\" tags.\nExample: ./botw.exe 8 1481934 Dover")
        (let%map_open.Core.Command root_level = anon ("boundary_admin_level" %: string)
        and root_relation = anon ("boundary_relation_id" %: string) 
        and root_name = anon ("boundary_name" %: string) 
        and write_dot = flag "-write-dot" no_arg ~doc:"export the Bigraph to a dot-file"
        and id_in_parameter = flag "-id-parameter" no_arg ~doc:"keep id in parameter instead of linking to seperate ID node" in
        fun () -> main root_level root_relation root_name write_dot id_in_parameter)

let () = 
    (* Memtrace.trace_if_requested (); *)
    Command_unix.run command
