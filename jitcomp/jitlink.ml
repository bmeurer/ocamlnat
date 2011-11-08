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

open Misc

type error =
    Cannot_generate_cmxs
  | File_not_found of string
  | Unsupported_file of string
  | Undefined_global of string
  | Dynamic_linking_failed of string * Dynlink.error

exception Error of error

(* Load in-core a .cmxs file *)

let loadfile name0 =
  try
    let name = (try find_in_path !Config.load_path name0
                with Not_found -> raise (Error(File_not_found name0))) in
    if Filename.check_suffix name ".cmxs" then begin
      (* .cmxs files can be loaded directly *)
      Dynlink.loadfile name
    end else if Filename.check_suffix name ".cmxa" then begin
      (* Need to generate a temporary .cmxs file first *)
      let temp = (Filename.basename (Filename.chop_extension name0)) in
      let cmxs = Filename.temp_file temp ".cmxs" in
      let cmd = Printf.sprintf "%s -linkall -shared -o %s %s"
                  (Filename.quote Config.standard_ocamlopt)
                  (Filename.quote cmxs)
                  (Filename.quote name) in
      if Ccomp.command cmd != 0 then raise (Error Cannot_generate_cmxs);
      try_finally
        (fun () -> Dynlink.loadfile cmxs)
        (fun () -> try Sys.remove cmxs with Sys_error _ -> ())
    end else
      raise (Error(Unsupported_file name))
  with
    Dynlink.Error error -> raise (Error(Dynamic_linking_failed(name0, error)))

(* Error report *)

open Format

let report_error ppf = function
  | Cannot_generate_cmxs ->
      fprintf ppf "Failed to generate temporary cmxs file"
  | File_not_found name ->
      fprintf ppf "File not found `%s'" name
  | Unsupported_file name ->
      fprintf ppf "Unsupported file `%s'" name
  | Undefined_global s ->
      fprintf ppf "Reference to undefined global `%s'" s
  | Dynamic_linking_failed(name, error) ->
      fprintf ppf "Error while loading `%s' - %s"
        name (Dynlink.error_message error)
