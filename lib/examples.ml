open Bigraph

let agent = Big.ion (Link.parse_face ["connection"]) (Ctrl.C ("Agent", [], 1))
let boundary = Big.ion (Link.Face.empty) (Ctrl.C ("Boundary", [], 0))
let street = Big.ion (Link.Face.empty) (Ctrl.C ("Street", [], 0))
let building = Big.ion (Link.Face.empty) (Ctrl.C ("Building", [], 0))
let site = Big.split 1
let disconnected_agent = Big.close (Link.parse_face ["connection"]) agent
let agent_a = Big.nest agent (Big.atom Link.Face.empty (Ctrl.C ("ID", [S "A"],0)))

module MSSolver = Solver.Make_SAT(Solver.MS)
module BRS =  Brs.Make(MSSolver)

let write_state n b = 
    let file = "renders/state"^(string_of_int n)^".dot" in 
    let _ = Lwt_main.run (Overpass.write_to_file (Bigraph.Big.to_dot b "b") file) in 
    let _ = Sys.command ("dot "^file^" -Tsvg -O") in 
    let _ = Sys.command ("rm "^file) in ()

(** example bigraph*)
let test = 
  Big.nest 
    boundary 
    (Big.par_of_list [Big.atom Link.Face.empty (Ctrl.C ("ID", [S "6-180837-Cambridgeshire"], 0)); site ;(Big.nest 
       boundary 
       (Big.par_of_list [Big.atom Link.Face.empty (Ctrl.C ("ID", [S "8-295355-Cambridge"],0)); site ;(Big.nest 
          street 
          (Big.par_of_list [Big.atom Link.Face.empty (Ctrl.C ("ID", [S "J J Thompson Avenue"],0)); site ; Big.nest
             building
             (Big.par 
                site
                (Big.atom Link.Face.empty (Ctrl.C ("ID", [S "8388-William Gates Building"], 0)))
             )]
          ))]
       ))]
    );;

let example_manual_change_lhs = 
    Big.par (Big.nest building (Big.par site (Big.atom Link.Face.empty (Ctrl.C ("ID", [S "B"], 0))))) agent_a
let react_example_manual_change = 
    let rhs =  Big.nest building (Big.par_of_list [site; Big.atom Link.Face.empty (Ctrl.C ("ID", [S "B"], 0)); agent_a;]) in
    BRS.parse_react_unsafe ~name:"Move down boundary to boundary"  ~lhs:example_manual_change_lhs ~rhs:rhs () None

let example_connect_agents_lhs = Big.par disconnected_agent disconnected_agent
let example_connect_agents_rhs = 
    Big.close 
        (Link.parse_face ["connection"])
        (Big.par agent agent)

