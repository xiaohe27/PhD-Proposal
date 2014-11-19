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
open Predicate
open MFOTL


type tuple = cst list 



(* compare two tuples *)
let compare t1 t2 = compare t1 t2
  (* this is Pervasives.compare: [2]>[1;4] (as 2>1), [2]<[3;4] (as 2<3),
     and [2]<[2;4] (as []<l for any non-empty l) *)

(*** operations on tuples ***)

let make_tuple x = x

let make_tuple2 sl tl = 
  List.map2 
    (fun s (_, t) -> 
      match t with
	| TInt -> Int (int_of_string s)
	| TStr -> Str s
    )
    sl tl
  
let get_constants t = t  



let get_at_pos pos t = List.nth t pos

let add_first t v = v::t
let add_last t v = t@[v]


let duplicate_pos pos t = 
  let v =  get_at_pos pos t in
    add_last t v


let project_away = Misc.remove_positions
let projections = Misc.get_positions



let satisfiesp p t = 
  (* we use same idea as Samuel: assign values to variables *)
  let rec satisf assign t a = 
    match t,a with
      | [],[] -> 
	  true, List.rev (snd (List.split assign))
      | ht::tt,ah::at -> 
	  (match ah with
	     | Var x ->
		 (try
		    let v = List.assoc x assign in
		      if v = ht then
			satisf assign tt at
		      else
			(false,[])
		  with Not_found -> 
		    satisf ((x,ht)::assign) tt at)
	     | Cst v -> 
		 if v = ht then
		   satisf assign tt at 
		 else
		   (false,[])
	  )
      | _ -> failwith "[Tuple.satisfiesp] The arity of [p] and the length of [t] differ."
  in 
    satisf [] t (Predicate.get_args p)



let satisfiesf1 f pos t = 
  f (get_at_pos pos t)


let satisfiesf2 f pos1 pos2 t = 
   f (get_at_pos pos1 t) (get_at_pos pos2 t)




exception Not_joinable


let join posval t1 t2 = 
  let rec join' crtpos pv t = 
    match pv,t with
      | (hp,hv)::tpv, ht::tt -> 
	  if hp = crtpos then
	    if hv = ht then
	      join' (crtpos+1) tpv tt
	    else
	      raise Not_joinable
	else
	  ht::(join' (crtpos+1) pv tt)
      | [],_ -> t
      | _,[] -> failwith "[Tuple.join] bad posval list"
  in
  t1 @ (join' 0 posval t2)

(* the result should be the same as [join t1 t2] *)
(* [join'] just checks that values in [t1] are correspct with respect
   to [posval], but does not select elements, while [join''] does not
   check anything, just selects positions that don't appear in [pos] *)
let join_rev pos2 posval t2 t1 = 
  let rec check crtpos pv t = 
    match pv,t with
      | (hp, hv)::tpv, ht::tt -> 
	if hp = crtpos then
	  if hv = ht then
	    check (crtpos+1) tpv tt
	  else
	    raise Not_joinable
	else
	  check (crtpos+1) pv tt
      | [], _ -> ()
      | _, [] -> failwith "[Tuple.join] bad posval list"
  in  
  let rec sel crtpos pl t = 
    match pl,t with
      | hp::tpv, ht::tt -> 
	if hp = crtpos then
	  sel (crtpos+1) tpv tt
	else
	  ht :: (sel (crtpos+1) pl tt)
      | [], _ -> t
      | _, [] -> failwith "[Tuple.join_rev] bad pos list"
  in  
  check 0 posval t1;
  t1 @ (sel 0 pos2 t2)
    


(** printing functions **)

let string_of_tuple = Misc.string_of_list string_of_cst
let print_tuple = Misc.print_list print_cst








