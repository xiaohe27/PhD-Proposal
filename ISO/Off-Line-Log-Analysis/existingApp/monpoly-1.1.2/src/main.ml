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
open Formula_parser
open Lexing
open Algorithm
open Log
open Rewriting


(********** testing *************)


let usage_string = 
  "Usage: monpoly -sig <file> -formula <file> [-negate] [-log <file>]
               [-help] [-version] [-debug <unit>] [-verbose] 
               [-check] [-sigout] [-unix] [-mem] [-nonewlastts]
               [-nofilterrel] [-nofilteremptytp] [-testfilter]"



let formulafile = ref ""   

let analyse_formulafile () = 
  let ic = open_in !formulafile in
  try 
    let f = Formula_parser.formula Formula_lexer.token (Lexing.from_channel ic) in
    if Misc.debugging Dbg_all then
      Printf.eprintf "[Main.main] The formula file was parsed correctly.\n";
    f
  with e -> Printf.eprintf "[Main.main] Failed to parse formula file\n"; raise e 
    
  



let logfile = ref ""   
let sigfile = ref ""   
let debug = ref ""

let negate = ref false
let inc = ref false
let memarg = ref false
let memarg2 = ref false
let sigout = ref false

let testfilteropt = ref false
let nofilteremptytpopt = ref false
let nofilterrelopt = ref false



let sigusr1_handler =
  (Sys.Signal_handle
     (fun _ ->
        print_endline "SIGUSR1 handler: exiting...";
        exit 0))
    
let sigusr2_handler =
  (Sys.Signal_handle
     (fun _ -> Misc.usr2 := true ))

let sigalrm_handler =
  (Sys.Signal_handle
     (fun _ -> Misc.alrm := true ))


let displayver = ref false

let version = Version.ver


let main () = 
  Sys.set_signal Sys.sigusr1 sigusr1_handler;
  Sys.set_signal Sys.sigusr2 sigusr2_handler;
  Sys.set_signal Sys.sigalrm sigalrm_handler;
  
  Misc.split_debug !debug;

  if !displayver then
    print_endline ("MonPoly, version " ^ version)
  else if !formulafile = "" then
    print_endline usage_string
  else 
    (* read formula file *)
    let f = analyse_formulafile () in
    let f = if !negate then Neg f else f in
    if !sigfile = "" then
      print_endline usage_string
    else
      begin
	(* read signature file *)
	let sign = Log.get_signature !sigfile in
	let is_mon, pf, vartypes = Rewriting.check_formula sign f in
	if !sigout then
	  Predicate.print_vartypes_list vartypes
	else if is_mon && not !Misc.checkf then
          begin
            if not !nofilterrelopt then
              Filter_rel.enable pf;
            if not !nofilteremptytpopt then
              Filter_empty_tp.enable pf;
            if !testfilteropt then
              Algorithm.test_filter !logfile pf
            else
	      (* start monitoring *)
              Algorithm.monitor !logfile pf
        end
      end

let _ = 
  Arg.parse [
    "-sig", Arg.Set_string sigfile, "\t\tChoose the signature file";
    "-formula", Arg.Set_string formulafile, "\tChoose the formula file"; 
    "-negate", Arg.Set negate, "\tAnalyze the negation of the input formula";
    "-log", Arg.Set_string logfile, "\t\tChoose the log file";
    "-version", Arg.Set displayver, "\tDisplay the version (and exit)";
    "-debug", Arg.Set_string debug, "\tChoose unit to debug";
    "-verbose", Arg.Set Misc.verbose, "\tTurn on verbose mode";
    "-check", Arg.Set Misc.checkf, "\tCheck if formula is monitorable (and exit)";
    "-sigout", Arg.Set sigout, "\tShow the output signature (and exit)";
    "-unix", Arg.Set MFOTL.unixts, "\tTimestamps represent Unix time";
    "-mem", Arg.Set memarg, "\t\tShow memory usage on stderr";
    "-mem2", Arg.Set memarg2, "\t\tShow memory usage on stderr";
    "-nonewlastts", Arg.Clear Misc.new_last_ts, "\tDo not add a last maximal time-stamp";
    "-nofilterrel", Arg.Set nofilterrelopt, "\tDisable filter_rel module";
    "-nofilteremptytp", Arg.Set nofilteremptytpopt, "\tDisable filter_empty_tp module";
    "-testfilter", Arg.Set testfilteropt, "\tTest filter on the log without evaluating the formula";
    ]
    (fun _ -> ())
    usage_string;
  if Misc.debugging Dbg_perf then
    ignore(Unix.alarm 600);
  main ();
  if !memarg then
    mem_all ()
  else if !memarg2 then
    prerr_endline (Misc.mem_current ())

