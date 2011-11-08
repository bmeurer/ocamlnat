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

(* Linker functionality *)

val loadfile: string -> unit

(* Error report *)

type error =
    Cannot_generate_cmxs
  | File_not_found of string
  | Unsupported_file of string
  | Undefined_global of string
  | Dynamic_linking_failed of string * Dynlink.error

exception Error of error

open Format

val report_error: formatter -> error -> unit
