(***********************************************************************)
(*                                                                     *)
(*                              ocamlnat                               *)
(*                                                                     *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,    *)
(*    Universität Siegen. All rights reserved. This file is distri-    *)
(*    buted under the terms of the Q Public License version 1.0.       *)
(*                                                                     *)
(***********************************************************************)

(* Command-line parameters *)

val include_dirs : string list ref
val no_std_include : bool ref
val print_types : bool ref
val debug : bool ref
val fast : bool ref
val classic : bool ref
val nopervasives : bool ref
val preprocessor : string option ref
val annotations : bool ref
val use_threads : bool ref
val use_vmthreads : bool ref
val noassert : bool ref
val verbose : bool ref
val noprompt : bool ref
val init_file : string option ref
val principal : bool ref
val recursive_types : bool ref
val strict_sequence : bool ref
val applicative_functors : bool ref
val gprofile : bool ref
val c_compiler : string option ref
val dump_parsetree : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dump_instr : bool ref
val optimize_for_speed : bool ref
val register_allocator : string ref
val dump_cmm : bool ref
val dump_selection : bool ref
val dump_live : bool ref
val dump_spill : bool ref
val dump_split : bool ref
val dump_interf : bool ref
val dump_prefer : bool ref
val dump_interval : bool ref
val dump_regalloc : bool ref
val dump_reload : bool ref
val dump_scheduling : bool ref
val dump_linear : bool ref
val dump_combine : bool ref
val native_code : bool ref
val inline_threshold : int ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
val shared : bool ref
val dlcode : bool ref
