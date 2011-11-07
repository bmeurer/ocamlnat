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

(* Common functions for jitting code *)

open Linearize

(* Execution *)

type evaluation_outcome = Result of Obj.t | Exception of exn

val jit_execsym: string -> evaluation_outcome
val jit_loadsym: string -> Obj.t

(* Code generation *)

val jit_text: unit -> unit
val jit_data: unit -> unit

val jit_label: label -> unit
val jit_symbol: string -> unit
val jit_global: string -> unit

type tag
val jit_symbol_tag: string -> tag
external jit_label_tag: label -> tag = "%identity"

type reloc =
    RelocAbs32 of tag (* 32bit absolute *)
  | RelocAbs64 of tag (* 64bit absolute *)
  | RelocRel32 of tag (* 32bit relative *)
val jit_reloc: reloc -> unit

val jit_int8: int -> unit
val jit_int8n: nativeint -> unit
val jit_int16: int -> unit
val jit_int16n: nativeint -> unit
val jit_int32: int -> unit
val jit_int32l: int32 -> unit
val jit_int32n: nativeint -> unit
val jit_int64: int -> unit
val jit_int64L: int64 -> unit
val jit_int64n: nativeint -> unit
val jit_ascii: string -> unit
val jit_asciz: string -> unit

val jit_fill: int -> int -> unit
val jit_align: int -> int -> unit

val data: Cmm.data_item list -> unit

val begin_assembly: unit -> unit
val end_assembly: unit -> unit

(* Error report *)

type error =
    Undefined_global of string

exception Error of error

open Format

val report_error: formatter -> error -> unit
