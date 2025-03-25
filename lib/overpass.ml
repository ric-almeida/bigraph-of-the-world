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

(** downloads osm file of all admin boundaries, buildings and streets in area defined by root_id. https://overpass-turbo.eu/s/20XK*)
let query_boundaries_buildings_streets (root_level : string) (root_id : string) (root_name :string) =
    let url = api^"?data=relation%28"^root_id^"%29%3Bmap%5Fto%5Farea%2D%3E%2Ea%3B%28relation%5B%22boundary%22%3D%22administrative%22%5D%28area%2Ea%29%28if%3At%5B%22admin%5Flevel%22%5D%3E"^root_level^"%29%3Bnwr%5B%22addr%3Astreet%22%5D%5B%22name%22%5D%28area%2Ea%29%3Bnwr%5B%22addr%3Astreet%22%5D%5B%22addr%3Ahousenumber%22%5D%28area%2Ea%29%3Bway%5B%22highway%22%7E%22%28motorway%7Ctrunk%7Cprimary%7Csecondary%7Ctertiary%7Cunclassified%7Cresidential%29%22%5D%28area%2Ea%29%3B%29%3Bout%20meta%3B%0A" in
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