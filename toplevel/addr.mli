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

(* $Id$ *)

(* Native addressing *)

include module type of Nativeint

external of_int64: int64 -> t = "%int64_to_nativeint"
(** Convert the given 64-bit integer (type [int64]) to an
   address (type [nativeint]). On 32-bit platforms, the
   64-bit integer is taken modulo 2{^32}. On 64-bit platforms,
   the conversion is exact. *)

external to_int64: t -> int64 = "%int64_of_nativeint"
(** Convert the given address (type [nativeint])
   to a 64-bit integer (type [int64]). *)

val add_int: t -> int -> t
