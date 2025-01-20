open Bigraph
open Core

module IonListMap = Map.Make(String)

(** example bigraph*)
let a0 =
  Big.nest
    (Big.ion (Link.parse_face ["a"]) (Ctrl.C ("A", [S "hello"],1)))
    (Big.nest
 (Big.ion (Link.Face.empty) (Ctrl.C ("Snd", [],0)))
 (Big.par
    (Big.nest
       (Big.ion (Link.parse_face ["a"; "v_a"]) (Ctrl.C ("M", [],2)))
       Big.one)
    (Big.nest
       (Big.ion (Link.Face.empty) (Ctrl.C ("Ready", [],0)))
       (Big.nest
    (Big.ion (Link.Face.empty) (Ctrl.C ("Fun",[],0)))
    Big.one))))

(** given a child to parent string-to-string map, returns a string-to-ion-list map of parent to list of children ions*)
let build_ions childToParent = 
   let rec build_ions_helper keyValueList (result_mp : Big.t list IonListMap.t)= 
      match keyValueList with
      | [] -> result_mp
      | (k, v)::xs -> let ion =(Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [S k],0))) in
      build_ions_helper xs (IonListMap.update result_mp v ~f:(function
      | None -> [ion]  (* If the key doesn't exist, create a new list with the value *)
      | Some values -> (ion :: values)  (* If it exists, prepend the new value to the list *)
   )) in
   build_ions_helper (Map.to_alist childToParent) IonListMap.empty

exception TagNotFound of string
module StringListMap = Map.Make(String)

let street_to_buildings town = 
   let osm = Osm_xml.Parser.parse_file ("data/buildings/"^town^".osm") in
   let relations_of_osm (Osm_xml.Types.OSM osm_record) = osm_record.ways in
   let relations = Map.to_alist (relations_of_osm osm) in
   let rec explore_children list result_mp=
      match list with
      | [] -> result_mp
      | (Osm_xml.Types.OSMId building ,Osm_xml.Types.OSMWay osm_relation_record)::xs ->let name= Osm_xml.Types.find_tag osm_relation_record.tags "name" in
         match name with
         | None -> raise (TagNotFound building)
         | Some name -> let street= Osm_xml.Types.find_tag osm_relation_record.tags "addr:street" in
            match street with
            | None -> raise (TagNotFound building)
            | Some street -> explore_children xs (StringListMap.update result_mp street ~f:(function
            | None -> [name]  (* If the key doesn't exist, create a new list with the value *)
            | Some values -> (name :: values)  (* If it exists, prepend the new value to the list *)
         )) in
   explore_children relations StringListMap.empty

exception InvalidBuildings of string

(** given a parent to child string-to-string map and a string root, builds the bigraph with that root*)
let rec build_place_graph parentToChildren root = 
   let ion =(Big.ion (Link.parse_face []) (Ctrl.C ("Boundary",[S root],0))) in
   match Map.find parentToChildren root with
   | Some l -> Big.nest ion (Big.par_of_list ((Big.split 1)::(List.map l ~f:(fun s -> build_place_graph parentToChildren s))))
   | None -> Big.nest ion 
      (Big.par_of_list ((Big.split 1)::
         (List.map (Map.to_alist (street_to_buildings root)) ~f:(fun (street,buildings) -> 
            Big.nest (Big.ion (Link.parse_face []) (Ctrl.C ("Street",[S street],0))) (Big.par_of_list ((Big.split 1)::(List.map buildings ~f:(fun building -> Big.ion (Link.parse_face []) (Ctrl.C ("Building",[S building],0))))))))))