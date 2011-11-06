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

(* Load packages from toploops and scripts *)

open Format
open Toploop
open Topmisc

let real_toploop = !Sys.interactive

let directories = ref ([Findlib.ocaml_stdlib()])
let forbidden = ref ([] : string list)
let loaded = ref ([] : string list)
let predicates = ref ([] : string list)

let add_directory d =
  let d = normalize_dirname d in
  if not (List.mem d !directories) then begin
    Topmisc.prepend_load_path d;
    directories := d :: !directories;
    if real_toploop then
      fprintf std_formatter "@[`%s' added to search path@]@." d
  end

let add_predicates pl =
  predicates := pl @@ !predicates

let syntax s =
  add_predicates ["syntax"; s]

let standard_syntax() =
  syntax "camlp4o"

let revised_syntax() =
  syntax "camlp4r"

let load pl =
  List.iter
    (fun p ->
       if not (List.mem p !loaded) then begin
         (* Determine the package directory *)
         let d = Findlib.package_directory p in
         add_directory d;
         (* Leave package out if forbidden *)
         if not (List.mem p !forbidden) then begin
           (* Determine the 'archive' property *)
           let archive = (try Findlib.package_property !predicates p "archive"
                          with Not_found -> "") in
           (* Split the 'archive' property and load the files *)
           let archives = split_in_words archive in
           List.iter
             (fun arch ->
                let arch = Findlib.resolve_path ~base:d arch in
                loadfile arch;
                if real_toploop then
                  fprintf std_formatter "@[`%s' loaded@]@." arch)
             archives
         end;
         (* The package is loaded *)
         loaded := p :: !loaded
       end)
    pl

let load_deeply pl =
  (* Load the sorted list of ancestors in turn *)
  load (Findlib.package_deep_ancestors !predicates pl)

let don't_load pl =
  forbidden := pl @@ !forbidden;
  (* Check if packages exist *)
  List.iter (ignore +++ Findlib.package_directory) pl

let don't_load_deeply pl =
  (* Check if packages exist *)
  List.iter (ignore +++ Findlib.package_directory) pl;
  (* Add the sorted list of ancestors to the forbidden packages *)
  don't_load (Findlib.package_deep_ancestors !predicates pl)

let protect f x =
  try ignore(f x)
  with Failure s -> fprintf err_formatter "@[%s@]@." s

let reset() =
  loaded := [];
  (* Add "#require" directive *)
  Hashtbl.replace
    directive_table
    "require"
    (Directive_string(protect (load_deeply +++ split_in_words)));
  (* Add "#predicates" directive *)
  Hashtbl.replace
    directive_table
    "predicates"
    (Directive_string(protect (add_predicates +++ split_in_words)));
  (* Add "#camlp4o" directive *)
  Hashtbl.replace
    directive_table
    "camlp4o"
    (Directive_none(protect (fun () ->
                               standard_syntax();
                               load_deeply ["camlp4"])));
  (* Add "#camlp4r" directive *)
  Hashtbl.replace
    directive_table
    "camlp4r"
    (Directive_none(protect (fun () ->
                               revised_syntax();
                               load_deeply ["camlp4"])));
  (* Add "#list" directive *)
  Hashtbl.replace
    directive_table
    "list"
    (Directive_none(protect (fun () ->
                               Findlib.list_packages stdout;
                               flush stdout)));
  (* Add "#thread" directive *)
  Hashtbl.replace
    directive_table
    "thread"
    (Directive_none(protect (fun () -> (*TODO*)())))

let announce() =
  if real_toploop then begin
    print_endline
      ("Findlib has been successfully loaded. Additional directives:\n" ^
       "  #require \"package\";;      to load a package\n" ^
       "  #list;;                   to list the available packages\n" ^
       "  #camlp4o;;                to load camlp4 (standard syntax)\n" ^
       "  #camlp4r;;                to load camlp4 (revised syntax)\n" ^
       "  #predicates \"p,q,...\";;   to set these predicates\n" ^
       "  Topfind.reset();;         to force that packages will be reloaded\n")
  end
