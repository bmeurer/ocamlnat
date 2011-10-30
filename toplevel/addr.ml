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

(* Native addressing *)

include Nativeint

external of_int64: int64 -> t = "%int64_to_nativeint"
external to_int64: t -> int64 = "%int64_of_nativeint"

let add_int x y =
  add x (of_int y)
