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
open Db


(** Normalization (see Sec.5.3.2) **)

let elim_double_negation f = 
  let rec elim = function
    | Card f -> Card (elim f)
    | Neg (Neg f) -> elim f
    | Neg f -> Neg (elim f)
    | And (f1, f2) -> And (elim f1, elim f2)
    | Or (f1, f2) -> Or (elim f1, elim f2)
    | Exists (v, f) -> Exists (v, elim f)
    | Prev (intv, f) -> Prev (intv, elim f)
    | Next (intv, f) -> Next (intv, elim f)
    | Eventually (intv, f) -> Eventually (intv, elim f)
    | Once (intv, f) -> Once (intv, elim f)
    | Always (intv, f) -> Always (intv, elim f)
    | PastAlways (intv, f) -> PastAlways (intv, elim f)
    | Since (intv, f1, f2) -> Since (intv, elim f1, elim f2) 
    | Until (intv, f1, f2) -> Until (intv, elim f1, elim f2)
    | Equal (t1, t2) -> Equal (t1, t2) 
    | Less (t1, t2) -> Less (t1, t2) 
    | Pred p -> Pred p
    | _ -> failwith "[Rewriting.elim_double_negation]"
  in 
    elim f


let tt = Equal (Cst (Int 0), Cst (Int 0))

(* This function:
   - eliminates the following syntactic sugar: Implies, Equiv, ForAll
   (see Def.5.3.1.(2))
   - pushes down negation (see Def.5.3.1.(b,c,d,d',f); not yet: e,g-j.)
   - considers the Always, and PastAlways operators as syntactic sugar
   and eliminates them
*)
let rec norm = function
  | Card f -> Card (norm f)
  | Neg (And (f1, f2)) -> Or ((norm (Neg f1)), (norm (Neg  f2)))
  | Neg (Or (f1, f2)) -> And ((norm (Neg f1)), (norm (Neg  f2)))
  | Neg (Prev (intv, f)) -> (Prev (intv, norm (Neg f))) 
  | Neg (Next (intv, f)) -> (Next (intv, norm (Neg f))) 
  | Neg (Eventually (intv, f)) -> 
      (* (Always (intv, norm (Neg f))) *)
      Neg (Eventually (intv, norm f))
  | Neg (Once (intv, f)) -> 
      (* (PastAlways (intv, norm (Neg f))) *)
      Neg (Once (intv, norm f))
  | Neg (Always (intv, f)) -> 
      (Eventually (intv, norm (Neg f)))
  | Neg (PastAlways (intv, f)) -> 
      (Once (intv, norm (Neg f)))
  | Neg (Implies (f1, f2) as f) -> norm (Neg (norm f))
  | Neg (Equiv (f1, f2) as f) -> norm (Neg (norm f))
  | Neg f -> Neg (norm f)

  | Equal (t1, t2) -> Equal (t1, t2) 
  | Less (t1, t2) -> Less (t1, t2) 
  | Pred p -> Pred p

  | And (f1, f2) -> And (norm f1, norm f2) 
  | Or (f1, f2) -> Or (norm f1, norm f2) 
  | Exists (v, f) -> 
      let f = norm f in
	if List.mem v (MFOTL.free_vars f) then
	  Exists (v, f)
	else
	  f

  | Implies (f1, f2) -> Or (norm (Neg f1), norm f2) 
  | Equiv (f1, f2) -> And
      (Or (norm (Neg f1), norm f2),  
       Or (norm f1, norm (Neg f2)))
  | ForAll (v, f) -> norm (Neg (Exists (v, Neg f)))

  | Prev (intv, f) -> Prev (intv, norm f)
  | Next (intv, f) -> Next (intv, norm f)

  | Eventually (intv, f) -> Eventually (intv, norm f)
  | Once (intv, f) -> Once (intv, norm f)
  | Always (intv, f) -> Neg (Eventually (intv, norm (Neg f)))
  | PastAlways (intv, f) -> Neg (Once (intv, norm (Neg f)))

  | Since (intv, f1, f2) -> Since (intv, norm f1, norm f2) 
  | Until (intv, f1, f2) -> Until (intv, norm f1, norm f2)



(* The function [normalize] pushes down negations and eliminates
   double negations. *)
let normalize f = 
  elim_double_negation (norm f)



(** (TSF) Safe-range Check (see Sec. 5.3.3, 5.3.5) **)

(** returns [(rrv,b)] where [rrv] is the set of ``range restricted''
    variables, with the following difference from the thesis: for a
    subformula [f=Exists (v,f')], [rr f := (rr f') - {v}]. In the
    thesis, the [rr] function is not defined when [v] is not range
    restricted in [f]; when this is the case we set [b] to [false]. So
    when [b=true] then the Samuel's [rr] function is defined (and the
    set of range restricted variables, in our case and in his case,
    coincide. *)

let rec rr = function
  | Card f -> rr f 

  | Pred p -> (Predicate.pvars p, true)

  | Equal (t1, t2) ->
      (match t1, t2 with 
	 | Var x, Cst c -> ([x], true)
	 | Cst c, Var x -> ([x], true)
	 | _ -> ([], true) )
  | Less (t1, t2) -> 
      (match t1, t2 with 
	 | Var x, Var y when x=y -> ([x], true)
	 | Var x, Cst c -> ([x], true)
	 | _ -> ([], true))

  | Neg (Equal (t1, t2)) ->
      (match t1, t2 with 
	 | Var x, Var y when x=y -> ([x], true)
	 | _ -> ([], true))
  | Neg (Less (t1, t2)) -> 
      (match t1, t2 with 
	 | Cst c, Var x -> ([x], true)
	 | _ -> ([], true))

  | Neg f -> 
    let _, b = rr f in
    ([], b)

  | And (f1, Equal (Var x, Var y)) -> 
      let (rr1, b) = rr f1 in
	if List.mem x rr1 then
	  (Misc.union rr1 [y], b)
	else if List.mem y rr1 then
	  (Misc.union rr1 [x], b)
	else
	  (rr1, b)

  | And (f1, Less (Var x, Var y)) -> 
      let (rr1, b) = rr f1 in
	if List.mem y rr1  || x = y then
	  (Misc.union rr1 [x], b)
	else
	  (rr1, b)

  | And (f1, Neg (Less (Var x, Var y))) ->
      let (rr1, b) = rr f1 in
	if List.mem x rr1 then
	  (Misc.union rr1 [y], b)
	else
	  (rr1, b)

  | And (f1, f2) -> 
      let (rr1, b1) = rr f1 in
      let (rr2, b2) = rr f2 in
	(Misc.union rr1 rr2, b1 && b2)

  | Or (f1, f2) -> 
      let (rr1, b1) = rr f1 in
      let (rr2, b2) = rr f2 in
	(List.filter (fun v -> List.mem v rr1) rr2, b1 && b2)

  | Exists (v, f) -> 
      let (rrf, b) = rr f in
	if List.mem v rrf then
          (List.filter (fun x -> x<>v) rrf, b)
	else
	  (rrf, false)

  | Prev (intv, f) -> rr f
  | Next (intv, f) -> rr f
  | Eventually (intv, f) -> rr f 
  | Once (intv, f) -> rr f

  | Since (intv, f1, f2)  
  | Until (intv, f1, f2) -> 
    let _, b1 = rr f1 in
    let rr2, b2 = rr f2 in
    (rr2, b1 && b2)

  | _ -> failwith "[Rewriting.rr]"

    

let is_saferange f = 
  let rrv, b = rr f in
    b &&
      (
	let rv = List.sort compare rrv in
	let fv = List.sort compare (MFOTL.free_vars f) in
	  rv = fv
      )


    
let is_tsfsaferange f = 
  let rec is_tsfsr f = 
    let recb = 
      Misc.conjunction (
	List.map is_tsfsr (MFOTL.direct_subformulas f))
    in
      if MFOTL.is_temporal f then
	recb && (is_saferange f) && 
	  (Misc.conjunction 
	     (List.map is_saferange (MFOTL.direct_subformulas f))) 
      else
	recb      
  in
    (is_saferange f) && (is_tsfsr f)



(* This function tells us beforehand whether a formula is monitorable
   by MonPoly. It should thus exactly correspond to the
   implementation of the Algorithm module. 
*)
(* Remark: There are a few formulae that are not TSF safe-range
   (according strictly to the given definition), but which are
   monitorable; and we could accept even a few more: see
   examples/test4.mfotl. However, there are many formulae which are
   TSF safe range but not monitorable since our propagation function
   is still quite limited. 
*)
let rec is_monitorable = function
  | Card f -> is_monitorable f

  | Equal (t1, t2) -> 
      (match t1, t2 with
	 | Var _, Var _ -> false
	 | _ -> true
      )

  | Less (t1, t2) -> 
      (match t1, t2 with
	 | Var x, Var y -> (x=y)
	 | Cst _, Var _ -> false
	 | Var _, Cst (Str _) -> false
	 | _ -> true
      )
	
  | Pred p -> true

  | Neg f -> 
      (match f with 
	 | Equal (Cst _, Cst _) -> true
	 | Less (Cst (Int _), Var _) -> true
	 | _ ->  is_monitorable f && MFOTL.free_vars f = [] 
      )

  | And (f1, f2) -> 
      let fv1 = MFOTL.free_vars f1 in
	(is_monitorable f1)
	&&
	  (match f2 with
	     | Equal (t1, t2) -> 
		 (match t1, t2 with
		    | Var x, Var y -> (List.mem x fv1) || (List.mem y fv1)
		    | _ -> true
		 )
	     | Less (t1, t2) -> 	 
		 (match t1, t2 with
		    | Var x, Var y -> (x = y) || (List.mem y fv1)
		    | Cst c, Var y -> List.mem y fv1
		    | _ -> true
		 )
	     | Neg (Equal (Var x, Var y)) -> 
		 (x=y) || ((List.mem x fv1) && (List.mem y fv1))

	     | Neg (Less (Var x, Var y)) -> List.mem x fv1
	     | Neg (Less (Cst (Int _), Var y)) -> true
		 
	     | Neg f2' -> 
		 (is_monitorable f2') && 
		   Misc.subset (MFOTL.free_vars f2') fv1
	     | _ -> is_monitorable f2
	  )

  | Or (f1, f2) -> 
    let fv1 = MFOTL.free_vars f1 in
    let fv2 = MFOTL.free_vars f2 in
    (Misc.subset fv1 fv2) && (Misc.subset fv2 fv1) && 
      (is_monitorable f1) && (is_monitorable f2)

  | Exists (v, f) -> 
      assert (List.mem v (MFOTL.free_vars f));
      is_monitorable f

  | Prev (_, f)
  | Next (_, f) -> is_monitorable f

  | Since (intv, f1, f2)
  | Until (intv, f1, f2) -> 
      (is_monitorable f2)
      &&
	(match f1 with
	   | Neg f1' -> 
	       (is_monitorable f1') && (
		 let fv1' = MFOTL.free_vars f1' in
		 let fv2 = MFOTL.free_vars f2 in
		   Misc.subset fv1' fv2
	       )
	   | _ -> is_monitorable f1 &&
	       (
		 let fv1 = MFOTL.free_vars f1 in
		 let fv2 = MFOTL.free_vars f2 in
		   Misc.subset fv1 fv2
	       )
	)

  | Eventually (_, f2)
  | Once (_, f2) -> is_monitorable f2

  | _ -> false

(* These operators should have been eliminated:
  | Implies (f1, f2) -> 
  | Equiv (f1, f2) -> 
  | ForAll (v, f) -> 
  | Always (intv, f) -> 
  | PastAlways (intv, f) -> 
*)



(** Propagation of Range Restrictions (see Sec.5.3.4) **)

let propagate_cond f1 f2 = 
  let rr1, b1 = rr f1 in
  let rr2, b2 = rr f2 in
  let fv2 = MFOTL.free_vars f2 in
    Misc.inter rr1 (Misc.diff fv2 rr2) <> []
	    

let rec propagate = function
  | And (f', And (f1, f2)) -> (* not a rule of Sec.5.3.4 *)
      if propagate_cond f' f1 then
        propagate (And (propagate (And (f', f1)), propagate f2))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
	let f2 = propagate f2 in
          And (f', And (f1, f2))

  | And (f', Or (f1, f2)) -> 
      if propagate_cond f' f1 then
        Or (propagate (And (f', f1)), propagate (And (f', f2)))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
	let f2 = propagate f2 in
          And (f', Or (f1, f2))

  | And (f', Exists (v, f1)) -> 
      if propagate_cond f' f1 then
        And (f', Exists (v, propagate (And (f', f1))))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
          And (f', Exists (v, f1))

  | And (f', Neg (f1)) ->
      if propagate_cond f' f1 then
        And (f', Neg (propagate (And (f', f1))))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
          And (f', Neg (propagate f1))

  | And (f', Prev (intv, f1)) -> 
      if propagate_cond f' f1 then
        And (f', Prev (intv, propagate (And (Next (intv, f'), f1))))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
          And (f', Prev (intv, f1))

  | And (f', Next (intv, f1)) -> 
      if propagate_cond f' f1 then
        And (f', Next (intv, propagate (And (Prev (intv, f'), f1))))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in	
          And (f', Next (intv, f1))

  | And (f', Eventually (intv, f1)) -> 
      if propagate_cond f' f1 then
        And (f', Eventually (intv, propagate (And (Once (intv, f'), f1))))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
          And (f', Eventually (intv, f1))

  | And (f', Once (intv, f1)) when (snd intv <> Inf) ->
      if propagate_cond f' f1 then
        And (f', Once (intv, propagate (And (Eventually (intv, f'), f1))))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
          And (f', Once (intv, f1))

  | And (f', Since (intv, f1, f2)) when (snd intv <> Inf) -> 
      if propagate_cond f' f1 then
	let f1' = propagate (And (Eventually (intv, f'), f1)) in
          And (f', Since (intv, f1', f2))
      else if propagate_cond f' f2 then
	let f2' = propagate (And (Eventually (intv, f'), f2)) in
          And (f', Since (intv, f1, f2'))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
	let f2 = propagate f2 in
          And (f', Since (intv, f1, f2))

  | And (f', Until (intv, f1, f2)) -> 
      if propagate_cond f' f1 then
	let f1' = propagate (And (Once (intv, f'), f1)) in
          And (f', Until (intv, f1', f2))
      else if propagate_cond f' f2 then
	let f2' = propagate (And (Once (intv, f'), f2)) in
          And (f', Until (intv, f1, f2'))
      else
	let f' = propagate f' in
	let f1 = propagate f1 in
	let f2 = propagate f2 in
          And (f', Until (intv, f1, f2))

  | Neg f -> 
      Neg (propagate f)

  | And (f1, f2) -> And (propagate f1, propagate f2)
  | Or (f1, f2) -> Or (propagate f1, propagate f2)
  | Exists (v, f) -> Exists (v, propagate f)
  | Prev (intv, f) -> Prev (intv, propagate f)
  | Next (intv, f) -> Next (intv, propagate f)
  | Eventually (intv, f) -> Eventually (intv, propagate f)
  | Once (intv, f) -> Once (intv, propagate f)
  | Always (intv, f) -> Always (intv, propagate f)
  | PastAlways (intv, f) -> PastAlways (intv, propagate f)
  | Since (intv, f1, f2) -> Since (intv, propagate f1, propagate f2) 
  | Until (intv, f1, f2) -> Until (intv, propagate f1, propagate f2)

  | ForAll (v, f) -> ForAll (v, propagate f)

  | f -> f


(*** Some syntactic checks *)

let check_bound intv = 
  let _,b = intv in
  match b with
    | Inf -> false
    | _ -> true

let rec check_bounds = function
  | Equal _
  | Less _   
  | Pred _ 
    -> true

  | Card f 
  | Neg f 
  | Exists (_, f) 
  | ForAll (_, f)
  | Prev (_, f)
  | Next (_, f) 
  | Once (_, f)
  | PastAlways (_, f)
    -> check_bounds f

  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Equiv (f1, f2) 
  | Since (_, f1, f2) 
    -> (check_bounds f1) && (check_bounds f2)

  | Eventually (intv, f)
  | Always (intv, f) 
    -> (check_bound intv) && (check_bounds f)

  | Until (intv, f1, f2) 
    -> (check_bound intv) && (check_bounds f1) && (check_bounds f2)

let rec is_future = function
  | Equal _
  | Less _   
  | Pred _ 
    -> false

  | Card f
  | Neg f 
  | Exists (_, f) 
  | ForAll (_, f)
  | Prev (_, f)
  | Next (_, f) 
  | Once (_, f)
  | PastAlways (_, f)
    -> is_future f

  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Equiv (f1, f2) 
  | Since (_, f1, f2) 
    -> (is_future f1) || (is_future f2)

  | Eventually (_, _)
  | Always (_, _) 
  | Until (_, _, _) 
    -> true


(* We check that 
  - any predicate used in the formula is declared in the signature
  - the number of arguments of predicates matches their arity
  - the formula type checks
 [check_syntax db_schema f] returns the list of free variables of [f]
 together with their types
*)
let rec check_syntax db_schema f = 
  let get_type p pos = 
    let vartype_list = List.assoc p db_schema in
    snd (List.nth vartype_list pos)
  in

  (* [assign] is an association list of tuples (variable,type) *)
  let rec check_vars p assign pos = function 
    | (Var v)::t -> 
      if List.mem_assoc v assign then
	  if get_type p pos <> List.assoc v assign then
	    let str = Printf.sprintf 
	      "[Rewriting.check_syntax] Type check error on variable at position %d in predicate %s." pos p	    
	    in failwith str
	  else
	    check_vars p assign (pos + 1) t
      else
	check_vars p ((v, get_type p pos) :: assign) (pos + 1) t

    | (Cst c)::t -> 
      (match c, get_type p pos with
	| Int _, TInt 
	| Str _, TStr -> check_vars p assign (pos + 1) t
	| _ -> let str = Printf.sprintf 
		 "[Rewriting.check_syntax] Type check error on constant at position %d in predicate %s." pos p	    
	       in failwith str
      )

    | [] -> assign
  in 

  let union assign1 assign2 = 
    assign2 @ 
      (List.filter 
	 (fun (x, xtype) -> 
	   if List.mem_assoc x assign2 then
	     if xtype = List.assoc x assign2 then
	       false
	     else
	       failwith (Printf.sprintf "[Rewriting.check_syntax] Type check error on variable %s." x)
	   else
	     true
	 ) 
	 assign1)
  in

  let rec check assign = function
    | Equal (t1,t2) 
    | Less (t1,t2) -> 
      (match t1, t2 with
	| Var x, Var y -> 
	  if List.mem_assoc x assign then
	    let xtype = List.assoc x assign in
	    if List.mem_assoc y assign then
	      if xtype <> List.assoc y assign then
		let str = Printf.sprintf 
		  "[Rewriting.check_syntax] The equality %s = %s does not type check." x y	    
		in failwith str
	      else
		assign
	    else
	      (y, xtype) :: assign
	  else
	    if List.mem_assoc y assign then
	      let ytype = List.assoc y assign in
	      (x, ytype) :: assign
	    else
	      (* Remark: not a complete check, as if both variables haven't
		 yet been assigned a type, then they might have one later on...*)
	      assign

	| Var x, Cst c 
	| Cst c, Var x ->
	  if List.mem_assoc x assign then
	    (match c, List.assoc x assign with
	      | Int _, TInt 
	      | Str _, TStr -> assign
	      | _ -> failwith ("[Rewriting.check_syntax] The equality " ^ 
				  x ^ " = " ^  
				  (Predicate.string_of_cst c) ^ "does not type check.")
	    )
	  else
	    let xtype = Predicate.type_of_cst c in
	    (x, xtype) :: assign

	| Cst c, Cst c' ->
	  (match c, c' with
	    | Int _, Int _ 
	    | Str _, Str _ -> assign
	    | _ -> let str = Printf.sprintf 
		     "[Rewriting.check_syntax] The equality %s = %s does not type check." 
		     (Predicate.string_of_cst c) (Predicate.string_of_cst c')
		   in failwith str
	  )
      )
      
    | Pred p -> 
      let name, ar, args = Predicate.get_info p in
      if List.mem_assoc name db_schema then
	begin
	  let vartype_list = List.assoc name db_schema in
	  if ar <> List.length vartype_list then
	    failwith ("[Rewriting.check_syntax] wrong arity for predicate " ^ name ^ " in input formula")
	end
      else
	failwith ("[Rewriting.check_syntax] unknown predicate " ^ name  ^ " in input formula");
      	
      check_vars name assign 0 args

    | Card f
    | Neg f 
    | Prev (_,f) 
    | Next (_,f) 
    | Eventually (_,f) 
    | Once (_,f) 
    | Always (_,f) 
    | PastAlways (_,f) -> check assign f

    | Exists (v,f)
    | ForAll (v,f) -> List.filter (fun (x,t) -> x <> v) (check assign f)

    | And (f1,f2) 
    | Or (f1,f2) 
    | Implies (f1,f2) 
    | Equiv (f1,f2) 
    | Since (_,f1,f2)  
    | Until (_,f1,f2) -> 
      let assign1 = check assign f1 in
      union assign1 (check assign1 f2)
  in
  List.rev (check [] f)


let check_formula s f =
  (* we first the formula's syntax *)
  ignore(check_syntax s f);

  (* we then check that it is a bounded future formula *)
  if not (check_bounds f) then
    begin
      print_endline "The formula contains an unbounded future temporal operator. \
                     It is hence not monitorable.";
      exit 1;
    end;
  
  let nf = normalize f in
  if (Misc.debugging Dbg_monitorable) && nf <> f then
    MFOTL.printnl_formula "The normalized formula is: " nf;

  let is_mon = is_monitorable nf in
  let pf = if is_mon then nf else propagate nf in
  if (Misc.debugging Dbg_monitorable) && pf <> nf then
    MFOTL.printnl_formula "The \"propagated\" formula is: " pf;

  (* by default, that is without user specification, we add a new
     maximal time-stamp for future formulas; that is, we assume that
     there no more events will happen in the future *)
  if not (is_future pf) then
    Misc.new_last_ts := false;

  if !Misc.verbose || !Misc.checkf then
    begin
      if pf <> f then
	MFOTL.printnl_formula "The input formula is: " f;
      
      MFOTL.print_formula "The analyzed formula is:\n" pf;	      
      (* print_string "The free variables are: "; *)
      print_string " ";
      Misc.print_list print_string (MFOTL.free_vars f);
      print_newline()
    end;

  let is_mon = if not is_mon then is_monitorable pf else is_mon in
  if (not is_mon) then
    begin
      print_string "The analysed formula is not monitorable.\n";
      let is_sr = is_saferange nf in
      assert(is_sr = is_saferange pf);
      if is_sr then	
	print_endline "However, the input (and also the analyzed) formula is safe-range, \n\
	   hence one should be able to rewrite it into a monitorable formula."	
      else
	print_endline "The analysed formula is neither safe-range.";
      let is_tsfsr = is_tsfsaferange pf in
      if is_tsfsr then
	print_endline "By the way, the analyzed formula is TSF safe-range."
      else
	print_endline "By the way, the analyzed formula is not TSF safe-range.";
    end
  else if !Misc.checkf then
    print_string "The analysed formula is monitorable.\n";
  
  (is_mon, pf, check_syntax s pf)


