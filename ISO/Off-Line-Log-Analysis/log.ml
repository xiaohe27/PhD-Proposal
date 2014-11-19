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



open Lexing
open Misc
open Log_parser
open Filter_empty_tp

let logfile = ref ""

(*
let get_log_all logfile = 
  let ic = open_in logfile in
  let lexbuf = Lexing.from_channel ic in
  try
    Log_parser.all Log_lexer.token lexbuf 
  with e -> 
    Printf.printf "[Log.get_log_all] Failed to parse log file. Error at line %d\n" lexbuf.lex_start_p.pos_lnum; 
    raise e 
*)

(* here I want to print the line, and then underneath the error, as in:
saldkj sadlkjslak error salkdj 
                  ^^^^^

   I need to know where the beginning of the buffer is with regard to the
   beginning of the file: this is lex_start_pos
*)

let print_line str buf i = 
  let line_end_pos = String.index_from buf i '\n' in
  (* Printf.printf "i=%d line_end_pos=%d" i line_end_pos; print_newline(); *)
  let line = String.sub buf i (line_end_pos - i) in
  print_endline (str ^ line)

let test lexbuf =
  let len = String.length lexbuf.lex_buffer in
  Printf.printf "len=%d buffer_len=%d\n" len  lexbuf.lex_buffer_len;
  Printf.printf "lex_abs_pos = %d\n\
                 lex_start_pos = %d\n\
                 lex_curr_pos = %d\n\
                 lex_last_pos = %d\n\
                 lex_last_action = %d\n\
                 lex_start_p: pos_lnum = %d, pos_bol = %d, pos_cnum = %d\n\
                 lex_curr_p: pos_lnum = %d, pos_bol = %d, pos_cnum = %d\n"
    lexbuf.lex_abs_pos 
    lexbuf.lex_start_pos 
    lexbuf.lex_curr_pos 
    lexbuf.lex_last_pos 
    lexbuf.lex_last_action 
    lexbuf.lex_start_p.pos_lnum lexbuf.lex_start_p.pos_bol lexbuf.lex_start_p.pos_cnum
    lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_p.pos_bol lexbuf.lex_curr_p.pos_cnum;
  let buf = lexbuf.lex_buffer in
  print_endline buf;
  print_line "lex_abs_pos: " buf lexbuf.lex_abs_pos;
  print_line "lex_start_pos: " buf lexbuf.lex_start_pos;
  print_line "lex_curr_pos: " buf lexbuf.lex_curr_pos;
  print_line "lex_last_pos: " buf lexbuf.lex_last_pos;
  print_line "start_p.pos_bol: " buf lexbuf.lex_start_p.pos_bol;
  print_line "start_p.pos_cnum: " buf lexbuf.lex_start_p.pos_cnum;
  print_line "curr_p.pos_bol: " buf lexbuf.lex_curr_p.pos_bol;
  print_line "curr_p.pos_cnum: " buf lexbuf.lex_curr_p.pos_cnum;
  print_newline ()




let show_error lexbuf =
  (* test lexbuf; *)
  let line_start = lexbuf.lex_start_p.pos_bol in
  let line_end = 
    try 
      String.index_from lexbuf.lex_buffer line_start '\n' 
    with
	Not_found -> (String.length lexbuf.lex_buffer) - 1
  in
  let line = String.sub lexbuf.lex_buffer line_start (line_end - line_start) in
  let spaces = String.make (lexbuf.lex_start_pos - lexbuf.lex_start_p.pos_bol) ' ' in
  let pointers = String.make (lexbuf.lex_curr_pos - lexbuf.lex_start_pos) '^' in
  line ^ spaces ^ "\n" ^ pointers



let get_signature sigfile = 
  let ic = open_in sigfile in
  let lexbuf = Lexing.from_channel ic in
  try
    let sign = Log_parser.signature Log_lexer.token lexbuf in
    if Misc.debugging Dbg_all then
      Printf.eprintf "[Log.get_sign_from_file] The signature file was parsed correctly.\n";
    sign
  with e -> 
    Printf.eprintf 
      "[Log.get_sign_from_file] Failed to parse signature file. Error at line %d:\n%s\n" 
      lexbuf.lex_start_p.pos_lnum
      lexbuf.lex_buffer;  
    raise e 


let log_open logfile =
  let ic = 
    if logfile = "" then
      stdin
    else
      open_in logfile
  in   
  Lexing.from_channel ic




let update lexbuf = 
  if Misc.debugging Dbg_log then
    Printf.eprintf "[Log.update] curr=%d\n%!" lexbuf.lex_curr_pos;
  if lexbuf.lex_curr_pos > 0 then
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1

let skipped_tps = ref 0
let last = ref false
let tp = ref 0

let get_next_entry lexbuf =
  let rec get_next_entry lexbuf =
    let log_entry = 
      try
        Log_parser.tsdb Log_lexer.token lexbuf
      with e ->
        (Printf.eprintf "[Log.get_next_entry_var] Unexpected exception : %s\n%!" 
           (Printexc.to_string e);
         Printf.eprintf 
           "[Log.get_next_entry_var] Failed to parse log file. Error at line %d:\n%s\n%!" 
           lexbuf.lex_start_p.pos_lnum 
           "";
       (* (show_error lexbuf); *)
         raise e)
    in
      match log_entry with
        | Some (ts, db) ->
          begin
	    update lexbuf;
	    incr tp;
            if !Filter_empty_tp.enabled && Db.is_empty db then
              (* skip empty db *)
              begin
                incr skipped_tps;
                get_next_entry lexbuf
              end
            else
	      Some (!tp - 1, ts, db)
          end
      | None -> 
        if Misc.debugging Dbg_filter then
	  Printf.eprintf "Filter_empty_tp: skipped: %d, notskipped: %d\n" 
	    !skipped_tps (!tp - !skipped_tps);

	if not !last && !Misc.new_last_ts then	
	  begin
	    last := true;
	    Some (!tp, MFOTL.ts_max, Db.make_db [])
	  end
	else
	  None

  in get_next_entry lexbuf
