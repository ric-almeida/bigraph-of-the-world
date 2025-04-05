open Lwt.Syntax
open Core

let api = "https://overpass-api.de/api/interpreter"
(*"https://overpass.osm.jp/api/interpreter"*)
(*"https://maps.mail.ru/osm/tools/overpass/api/interpreter"*)
(*"https://overpass.private.coffee/api/interpreter"*)

(** writes string to file*)
let write_to_file body file_path = 
    let f ch = Lwt_stream.iter_s (Lwt_io.write ch) (Lwt_stream.of_list [body]) in
    Lwt_io.with_file ~mode:Lwt_io.output file_path f

(** downloads from uri to file with name dest*)
let rec download (uri : Uri.t) (file_path : string) =
    let* resp, body = Cohttp_lwt_unix.Client.get uri in
    match resp.status with
    | `OK -> 
        let stream = Cohttp_lwt.Body.to_stream body in
        let f ch = Lwt_stream.iter_s (Lwt_io.write ch) stream in
        Lwt_io.with_file ~mode:Lwt_io.output file_path f
    | _ -> 
        let _ = print_endline ("Retrying download for "^file_path) in
        download uri file_path

(** downloads osm file of all admin boundaries, buildings and streets in area defined by root_id, and intersections between streets inside and outside area. https://overpass-turbo.eu/s/21JM *)
let query_boundaries_buildings_streets (root_level : string) (root_id : string) (root_name :string) =
    let url = api^"?data=relation%28"^root_id^"%29-%3E.bound%3B%0A.bound+map_to_area-%3E.a%3B%0A%0A%2F%2F+all+streets+in+area%0Away%5B%22highway%22~%22%5E%28motorway%7Ctrunk%7Cprimary%7Csecondary%7Ctertiary%7Cunclassified%7Cresidential%29%28%7C_link%29%24%22%5D%28area.a%29-%3E.streets%3B%0A%2F%2F+all+nodes+of+streets+in+area%0Anode%28w.streets%29-%3E.insidenodes%3B%0A%0A%2F%2F+all+streets+that+contains+insidenodes.+includes+those+outside+area%0Away%28bn.insidenodes%29%5B%22highway%22~%22%5E%28motorway%7Ctrunk%7Cprimary%7Csecondary%7Ctertiary%7Cunclassified%7Cresidential%29%28%7C_link%29%24%22%5D-%3E.allstreets%3B%0A%2F%2F+streets+containing+insidenodes+that+are+outside+area%0A%28.allstreets%3B+-+.streets%3B%29-%3E.outsidestreets%3B%0A%2F%2F+nodes+of+streets+outside+area%0Anode%28w.outsidestreets%29-%3E.outsidenodes%3B%0A%0A%28%0A++%2F%2F+boundaries+contained+in+area%0A++relation%5B%22boundary%22%3D%22administrative%22%5D%28area.a%29%28if%3A+t%5B%22admin_level%22%5D+%3E+"^root_level^"%29%3B%0A++%2F%2F+named+places+in+area+with+a+street+tag%0A++nwr%5B%22addr%3Astreet%22%5D%5B%22name%22%5D%28area.a%29%3B%0A++%2F%2F+places+in+area+with+street+and+house+number+tag%0A++nwr%5B%22addr%3Astreet%22%5D%5B%22addr%3Ahousenumber%22%5D%28area.a%29%3B%0A++%0A++.streets%3B%0A++%0A++%2F%2F+nodes+that+are+intersections+between+streets+inside+and+outside+area.+don%27t+have+%22addr%3Astreet%22+tag%0A++node.outsidenodes.insidenodes%3B%0A%29%3B%0Aout+meta%3B%0A&target=mapql" in
    let file_path = "data/"^root_level^"-"^root_id^"-"^root_name^".osm" in
    download (Uri.of_string url) file_path

exception TagNotFound of string * string

(** downloads osm file of all admin boundaries in area defined by root_id, and call queryBoundaries and queryBuildings on all child boundaries*)
let query_all_children (root_level : string) (root_id : string) (root_name :string) =
    let _ = print_endline "Querying Overpass API for osm files... will take a while due to rate limiting" in
    let _ = if not (Lwt_main.run (Lwt_unix.file_exists "data/")) then Core_unix.mkdir_p ~perm:0o755 "data/" in
    let _ = Lwt_main.run (query_boundaries_buildings_streets root_level root_id root_name) in
    let osm_file = "data/"^root_level^"-"^root_id^"-"^root_name^".osm" in
    let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file osm_file in
    let boundaries = 
        Map.filter 
            osm_record.relations 
            ~f:(fun (Osm_xml.Types.OSMRelation child_relation)->
                match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
                | Some _ -> true
                | None -> false) in
    let bar total =
        let open Progress.Line in
        list [ elapsed (); bar total; count_to total ] in
    Progress.with_reporter 
        (bar (Map.length boundaries)) 
        (fun report_progress ->
            let f (Osm_xml.Types.OSMId child_id, Osm_xml.Types.OSMRelation child_relation) =
                let _ = report_progress 1 in
                match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
                | None -> raise (TagNotFound ("admin_level", child_id))
                | Some child_level ->
                match Osm_xml.Types.find_tag child_relation.tags "name" with 
                | None -> raise (TagNotFound ("name",child_id))
                | Some child_name -> 
                    query_boundaries_buildings_streets child_level child_id child_name in
            Lwt_main.run (Lwt_list.iter_s f (Map.to_alist boundaries)))