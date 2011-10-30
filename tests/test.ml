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

open OUnit

let ocamlnat = ref "ocamlnat"

let readfile fn =
  let ic = open_in fn in
  begin try
    let n = in_channel_length ic in
    let b = Buffer.create n in
    Buffer.add_channel b ic n;
    close_in ic;
    b
  with exn ->
    close_in ic;
    raise exn
  end

let test_std ?(options = []) dirname filename =
  filename >:: begin fun () ->
    let path = Filename.concat dirname filename in
    let reference = readfile (path ^ ".reference") in
    let result = Buffer.create (Buffer.length reference) in
    assert_command
      ~foutput:(Stream.iter (Buffer.add_char result))
      ~use_stderr:true
      !ocamlnat
      (options @ [path ^ ".ml"]);
    assert_equal result reference
  end

let test_std_dir ?(options = []) dirname =
  dirname >:::
    (List.map
      (fun fn -> test_std ~options dirname (Filename.chop_suffix fn ".ml"))
      (List.filter
        (fun fn -> Filename.check_suffix fn ".ml")
        (Array.to_list (Sys.readdir dirname))))

let suite =
  "ocamlnat" >:::
  [
    test_std_dir "basic";
    test_std_dir "misc";
    test_std_dir ~options:["-unsafe"] "misc-unsafe";
  ]

let _ =
  run_test_tt_main
    ~arg_specs:["-ocamlnat",
                Arg.String (fun fn -> ocamlnat := fn),
                "fn Path to ocamlnat"]
    suite

