(*
 * This file is part of MONPOLY.
 *
 * Copyright (C) 2011 Nokia Corporation and/or its subsidiary(-ies).
 * Contact:  Nokia Corporation (Debmalya Biswas: debmalya.biswas@nokia.com)
 * 
 * Copyright (C) 2012 ETH Zurich.
 * Contact:  ETH Zurich (Eugen Zalinescu: eugen.zalinescu@inf.ethz.ch)
 * 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, version 2.1 of the
 * License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see
 * http://www.gnu.org/licenses/lgpl-2.1.html.
 *
 * As a special exception to the GNU Lesser General Public License,
 * you may link, statically or dynamically, a "work that uses the
 * Library" with a publicly distributed version of the Library to
 * produce an executable file containing portions of the Library, and
 * distribute that executable file under terms of your choice, without
 * any of the additional requirements listed in clause 6 of the GNU
 * Lesser General Public License. By "a publicly distributed version
 * of the Library", we mean either the unmodified Library as
 * distributed by Nokia, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU
 * Lesser General Public License. This exception does not however
 * invalidate any other reasons why the executable file might be
 * covered by the GNU Lesser General Public License.
 *)



open Misc
open Tuple
open Predicate
open MFOTL


module Tuple_set = Set.Make (
  struct type t = tuple 
	 let compare = Tuple.compare
  end)

include Tuple_set

type relation = Tuple_set.t


(*** printing functions ***)


let print_rel str rel = 
  print_string str;
  let rel' = Tuple_set.elements rel in
  Misc.print_list Tuple.print_tuple rel'

let print_rel4 str rel = 
  print_string str;
  let rel' = Tuple_set.elements rel in
  Misc.print_list4 Tuple.print_tuple rel'
  (* Tuple_set.iter (fun t -> Tuple.print_tuple t; print_string " ") rel *)

let print_reln str rel = 
  print_rel str rel;
  print_newline()

let print_bigrel rel = 
  let rel' = Tuple_set.elements rel in
  Misc.print_list3 Tuple.print_tuple rel'

let print_orel = function
  | None -> print_string "N"
  | Some rel -> print_rel "S" rel




(********************************)


let make_relation list = 
  let rec make acc = function
  | [] -> acc
  | h::t -> make (Tuple_set.add h acc) t
  in make Tuple_set.empty list 
  
  
let map f rel =
  let res = ref (Tuple_set.empty) in
    Tuple_set.iter 
      (fun t ->
	 res := Tuple_set.add (f t) !res
      ) 
      rel


let map f rel =
  Tuple_set.fold (fun t rel' -> Tuple_set.add (f t) rel') rel Tuple_set.empty






(********************************)

let is_empty rel = 
  Tuple_set.is_empty rel


(********************************)



 

(** [matches] gives the columns which should match in the two
    relations in form of a list of tuples [(pos2,pos1)]: column [pos2] in
    [rel2] should match column [pos1] in [rel1] *)
let natural_join matches1 matches2 rel1 rel2 = 
  let joinrel = ref Tuple_set.empty in
  let process_rel_tuple join_fun matches rel2 t1 =
    (* For each tuple in [rel1] we compute the columns (i.e. positions)
       in rel2 for which there should be matching values and the values
       tuples in rel2 should have at these columns.

       For instance, for [matches] = [(0,1);(1,2);(3,0)] and t1 =
       [6;7;9] we obtain [(0,7);(1,9);(3,6)]. That is, from [rel2] we
       will select all tuples which have 7 on position 0, 9 on
       position 1, and 6 on position 3.  *)
    let pv = List.map 
      (fun (pos2, pos1) -> (pos2, Tuple.get_at_pos pos1 t1))
      matches 
    in
      Tuple_set.iter 
	(fun t2 -> 
	   try 
	     let t = join_fun pv t1 t2 in
	       joinrel := Tuple_set.add t !joinrel	      
	   with
	       Not_joinable -> ()
	) 
	rel2
  in
  if Tuple_set.cardinal rel1 < Tuple_set.cardinal rel2 then
    let join_fun = Tuple.join in
    Tuple_set.iter (process_rel_tuple join_fun matches1 rel2) rel1
  else
    begin
      let pos2 = List.map fst matches1 in
      let join_fun = Tuple.join_rev pos2 in
      Tuple_set.iter (process_rel_tuple join_fun matches2 rel1) rel2
    end;
  !joinrel


(* Misc.subset attr1 attr2 *)
let natural_join_sc1 matches rel1 rel2 = 
  let joinrel = ref Tuple_set.empty in
  Tuple_set.iter (fun t2 -> 
    let t1 = Tuple.make_tuple (
      List.map 
	(* x is at pos1 in t1 and at pos2 in t2 *)
	(fun (pos1, pos2) -> Tuple.get_at_pos pos2 t2)
	matches)
    in
    if Tuple_set.mem t1 rel1 then
      joinrel := Tuple_set.add t2 !joinrel
  ) rel2;
  !joinrel

(* Misc.subset attr2 attr1 *)
let natural_join_sc2 matches rel1 rel2 = 
  let joinrel = ref Tuple_set.empty in
  Tuple_set.iter (fun t1 -> 
    let t2 = Tuple.make_tuple (
      List.map 
	(* x is at pos2 in t2 and at pos1 in t1 *)
	(fun (pos2, pos1) -> Tuple.get_at_pos pos1 t1)
	matches) 
    in
    if Tuple_set.mem t2 rel2 then
      joinrel := Tuple_set.add t1 !joinrel
  ) rel1;
  !joinrel



let cross_product rel1 rel2 = 
  natural_join [] [] rel1 rel2



(* not set difference, but the diff operator as defined in Abiteboul, page 89 *)
let minus posl rel1 rel2 = 
  Tuple_set.filter 
    (fun t -> 
      let t' = (Tuple.projections posl t) in
      not (Tuple_set.mem t' rel2)
    ) 
    rel1


      

(* given the "predicate formula" [p] and a relation [rel] ("having the
   same signature" as [p]), obtain the relation containing those
   tuples of [rel] which satisfy [p] *)
let selectp p rel = 
  let res = ref Tuple_set.empty in
    Tuple_set.iter 
      (fun t ->
	 let b,t' = Tuple.satisfiesp p t in 
	 if b then
	   res := add t' !res
      ) rel;
    !res



let selectf1 f pos rel = 
  Tuple_set.filter (Tuple.satisfiesf1 f pos) rel


(* we could have special cases when x=y and f is, for instance, = or  < *)
let selectf2 f pos1 pos2 rel = 
  Tuple_set.filter (Tuple.satisfiesf2 f pos1 pos2) rel




let duplicate_col pos rel = 
  map (Tuple.duplicate_pos pos) rel


    
let add_col pos g rel = 
  let res = ref Tuple_set.empty in
  Tuple_set.iter 
    (fun t -> 
       let relt = make_relation 
	 (List.map (Tuple.add_last t) (g (Tuple.get_at_pos pos t)))
       in
	 res := union !res relt 
    ) rel;
    !res



(* the columns in [posl] are eliminated *)
let project_away posl rel = 
  map (Tuple.project_away posl) rel









