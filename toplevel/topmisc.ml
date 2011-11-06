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

(* Miscellaneous toplevel types and functions *)

open Misc

type error =
    Cannot_generate_cmxs
  | File_not_found of string
  | Unsupported_file of string
  | Dynamic_linking_failed of string * Dynlink.error

exception Error of error

(* List functions *)

let rec (@@) s t =
  match s with
    [] -> t
  | x :: s -> let s = s @@ t in if List.mem x s then s else x :: s

let split_in_words s =
  let l = String.length s in
  let rec split i j =
    if j < l then
      let j' = succ j in
      match s.[j] with
        (' '|'\t'|'\n'|'\r'|',') ->
          let rem = split j' j' in
          if i < j then (String.sub s i (j - i)) :: rem else rem
      | _ -> split i j'
    else
      if i < j then [String.sub s i (j-i)] else []
  in
  split 0 0

(* Higher-order functions *)

let (+++) f g x = f (g x)

(* Filename functions *)

let normalize_dirname d =
  (* Converts the file name of the directory [d] to the normal form.
   * For Unix, the '/' characters at the end are removed, and multiple
   * '/' are deleted.
   * For Windows, all '/' characters are converted to '\'. Two
   * backslashes at the beginning are tolerated.
   *)
  let s = String.copy d in
  let l = String.length d in
  let norm_dir_unix() =
    for k = 1 to l - 1 do
      if s.[k] = '/' && s.[k-1] = '/' then s.[k] <- Char.chr 0;
      if s.[k] = '/' && k = l-1 then s.[k] <- Char.chr 0
    done
  in
  let norm_dir_win() =
    if l >= 1 && s.[0] = '/' then s.[0] <- '\\';
    if l >= 2 && s.[1] = '/' then s.[1] <- '\\';
    for k = 2 to l - 1 do
      if s.[k] = '/' then s.[k] <- '\\';
      if s.[k] = '\\' && s.[k-1] = '\\' then s.[k] <- Char.chr 0;
      if s.[k] = '\\' && k = l-1 then s.[k] <- Char.chr 0
    done
  in
  let expunge() =
    let n = ref 0 in
    for k = 0 to l - 1 do
      if s.[k] = Char.chr 0 then incr n
    done;
    let s' = String.create (l - !n) in
    n := 0;
    for k = 0 to l - 1 do
      if s.[k] <> Char.chr 0 then begin
	s'.[ !n ] <- s.[k];
	incr n
      end
    done;
    s'
  in
  match Sys.os_type with
      "Unix" | "Cygwin" -> norm_dir_unix(); expunge()
    | "Win32" -> norm_dir_win(); expunge()
    | _ -> failwith "This os_type is not supported"

(* Load in-core a .cmxs file *)

let prepend_load_path s =
  let d = expand_directory Config.standard_library s in
  Config.load_path := d :: (List.filter ((<>) d) !Config.load_path)

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
  | Dynamic_linking_failed(name, error) ->
      fprintf ppf "Error while loading `%s' - %s"
        name (Dynlink.error_message error)
