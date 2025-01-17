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

(** helper to build map of parent to ions in a tail recursive way*)
let rec build_ions_helper keyValueList (result_mp : Big.t list IonListMap.t)= 
  match keyValueList with
  | [] -> result_mp
  | (k, v)::xs -> let ion =(Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [S k],0))) in
  build_ions_helper xs (IonListMap.update result_mp v ~f:(function
  | None -> [ion]  (* If the key doesn't exist, create a new list with the value *)
  | Some values -> (ion :: values)  (* If it exists, prepend the new value to the list *)
))

(** given a child to parent string-to-string map, returns a string-to-ion-list map of parent to list of children ions*)
let build_ions childToParent = build_ions_helper (Map.to_alist childToParent) IonListMap.empty

exception NotFound of string

(** given a parent to child string-to-string map and a string root, builds the bigraph with that root*)
let rec assemble_ions parentToChildren root = 
   let ion =(Big.ion (Link.parse_face []) (Ctrl.C ("Boundary", [S root],0))) in
   match Map.find parentToChildren root with
   | None -> Big.nest ion Big.one
   | Some l ->  
   match l with 
      | [] -> Big.nest ion Big.one
      | x::[] -> Big.nest ion (assemble_ions parentToChildren x)
      | _ -> Big.nest ion (Big.par_of_list (List.map l ~f:(fun s -> assemble_ions parentToChildren s)))