open Bigraph

let agent = Big.atom (Link.parse_face ["agent_id";"connection"]) Ctrl.{ s = "Agent"; p = []; i = 2 }
let agent2 = Big.atom (Link.parse_face ["agent_id2";"connection"]) Ctrl.{ s = "Agent"; p = []; i = 2 }
let disconnected_agent = Big.close (Link.parse_face ["connection"]) agent
let disconnected_agent2 = Big.close (Link.parse_face ["connection"]) agent2
let boundary = Big.ion (Link.parse_face ["boundary_id"]) Ctrl.{ s = "Boundary"; p = []; i = 1 }
let street = Big.ion (Link.parse_face ["street_id"]) Ctrl.{ s = "Street"; p = []; i = 1 }
let building = Big.ion (Link.parse_face ["building_id"]) Ctrl.{ s = "Building"; p = []; i = 1 }
let site = Big.split 1

module MSSolver = Solver.Make_SAT(Solver.MS)
module BRS =  Brs.Make(MSSolver)

let write_dot n b = 
    let file = "output/renders/state"^(string_of_int n)^".dot" in 
    let _ = 
        if not (Lwt_main.run (Lwt_unix.file_exists "output/renders/")) 
            then Core_unix.mkdir_p ~perm:0o755 "output/renders/" in
    Lwt_main.run (Overpass.write_to_file (Bigraph.Big.to_dot b "") file)

let write_svg n b = 
    let file = "output/renders/state"^(string_of_int n)^".dot" in 
    let _ = write_dot n b in
    let _ = Sys.command ("dot "^file^" -Tsvg -O") in 
    let _ = Sys.command ("rm "^file) in ()

(** example bigraph*)
let cambridgeshire_minimal = 
    let wgb =
        (Big.close
            (Link.parse_face ["building_id"])
            (Big.ppar 
                (Big.atom (Link.parse_face ["building_id"]) Ctrl.{ s = "ID"; p = [S "r8388-William Gates Building"]; i = 1 })
                (Big.par
                        (Big.nest building Big.one)
                        site)
            )) in
    let cavendish_wgb = 
        Big.comp
            (Big.close
                (Link.parse_face ["building_id"])
                (Big.ppar 
                    (Big.par
                        (Big.atom (Link.parse_face ["building_id"]) Ctrl.{ s = "ID"; p = [S "w164365618-Cavendish Laboratory"]; i = 1 })
                        site
                    )
                    (Big.par
                        (Big.nest building Big.one)
                        site)
                ))
            wgb in
    let jjthompson = 
        Big.comp
            (Big.close
                (Link.parse_face ["street_id"])
                (Big.ppar 
                    (Big.par
                        (Big.atom (Link.parse_face ["street_id"]) Ctrl.{ s = "ID"; p = [S "J J Thompson Avenue"]; i = 1 })
                        site
                    )
                    (Big.par street site)))
            (Big.ppar cavendish_wgb site) in
    let cambridge = 
        Big.comp
            (Big.close
                (Link.parse_face ["boundary_id"])
                (Big.ppar 
                    (Big.par
                        (Big.atom (Link.parse_face ["boundary_id"]) Ctrl.{ s = "ID"; p = [S "8-295355-Cambridge"]; i = 1 })
                        site
                    )
                    (Big.par boundary site)))
            (Big.ppar jjthompson site) in
    let cambridgeshire = 
        Big.comp
            (Big.close
                (Link.parse_face ["boundary_id"])
                (Big.ppar 
                    (Big.par
                        (Big.atom (Link.parse_face ["boundary_id"]) Ctrl.{ s = "ID"; p = [S "6-180837-Cambridgeshire"]; i = 1 })
                        site
                    )
                    boundary))
            cambridge in
    cambridgeshire

let example_connect_agents_lhs = Big.par disconnected_agent disconnected_agent2
let example_connect_agents_rhs = 
    Big.close 
        (Link.parse_face ["connection"])
        (Big.par agent agent2)

