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

(* Common functions for jitting code *)

open Cmm
open Emitaux
open Linearize

external nj_addsym: string -> Addr.t -> unit = "camlnat_jit_addsym" "noalloc"
external nj_getsym: string -> Addr.t = "camlnat_jit_getsym"

external nj_getint16: string -> int -> int = "camlnat_jit_getint16" "noalloc"
external nj_getint32: string -> int -> int32 = "camlnat_jit_getint32"
external nj_getint64: string -> int -> int64 = "camlnat_jit_getint64"
external nj_putint16: string -> int -> int -> unit = "camlnat_jit_putint16" "noalloc"
external nj_putint32: string -> int -> int32 -> unit = "camlnat_jit_putint32" "noalloc"
external nj_putint64: string -> int -> int64 -> unit = "camlnat_jit_putint64" "noalloc"
external nj_malloc: int -> int -> Addr.t * Addr.t = "camlnat_jit_malloc"
external nj_memcpy: Addr.t -> string -> int -> unit = "camlnat_jit_memcpy" "noalloc"

(* Sections *)

type section =
  { mutable sec_buf: string;
    mutable sec_pos: int;
    mutable sec_addr: Addr.t }

let grow_section sec pos =
  let len = String.length sec.sec_buf in
  if pos > len then begin
    let buf = String.create (len * 2) in
    String.unsafe_blit sec.sec_buf 0 buf 0 pos;
    sec.sec_buf <- buf
  end;
  sec.sec_pos <- pos

let new_section() =
  { sec_buf = String.create 1024;
    sec_pos = 0;
    sec_addr = 0n }

let reset_section sec =
  sec.sec_buf <- String.create 1024;
  sec.sec_pos <- 0

let text_sec = new_section()
let data_sec = new_section()
let curr_sec = ref text_sec

let jit_text() = curr_sec := text_sec
let jit_data() = curr_sec := data_sec

(* Labels and symbols *)

let labels = ref ([] : (label * (section * int)) list)
let globals = ref ([] : string list)
let symbols = ref ([] : (string * (section * int)) list)

let addr_of_label lbl =
  let (sec, ofs) = List.assoc lbl !labels in
  Addr.add_int sec.sec_addr ofs

let addr_of_symbol sym =
  try
    (* Try or own symbols first *)
    let (sec, ofs) = List.assoc sym !symbols in
    Addr.add_int sec.sec_addr ofs
  with
    Not_found ->
      (* Fallback to the global symbol table *)
      nj_getsym sym

let symbol_name sym =
  let buf = Buffer.create (String.length sym) in
  String.iter
    (function
        ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char buf c
      | c -> Printf.bprintf buf "$%02x" (Char.code c))
    sym;
  Buffer.contents buf

let jit_label lbl =
  let sec = !curr_sec in
  labels := (lbl, (sec, sec.sec_pos)) :: !labels

let jit_symbol sym =
  let sec = !curr_sec in
  let sym = symbol_name sym in
  symbols := (sym, (sec, sec.sec_pos)) :: !symbols

let jit_global sym =
  let sym = symbol_name sym in
  globals := sym :: !globals

(* Tags *)

type tag = Obj.t

let addr_of_tag tag =
  if Obj.is_int tag
  then addr_of_label (Obj.obj tag)
  else addr_of_symbol (Obj.obj tag)

let jit_symbol_tag sym =
  Obj.magic (symbol_name sym)

external jit_label_tag: label -> tag = "%identity"

(* Relocations *)

type reloc =
    RelocAbs32 of tag (* 32bit absolute *)
  | RelocAbs64 of tag (* 64bit absolute *)
  | RelocRel32 of tag (* 32bit relative *)

let relocs = ref ([] : (section * int * reloc) list)

let jit_reloc reloc =
  let sec = !curr_sec in
  relocs := (sec, sec.sec_pos, reloc) :: !relocs

let patch_reloc (sec, ofs, rel) =
  match rel with
    RelocAbs32 tag ->
      let a = Addr.to_int32 (addr_of_tag tag) in
      let d = nj_getint32 sec.sec_buf ofs in
      let x = Int32.add a d in
      nj_putint32 sec.sec_buf ofs x
  | RelocAbs64 tag ->
      let a = Addr.to_int64 (addr_of_tag tag) in
      let d = nj_getint64 sec.sec_buf ofs in
      let x = Int64.add a d in
      nj_putint64 sec.sec_buf ofs x
  | RelocRel32 tag ->
      let t = addr_of_tag tag in
      let r = Addr.add_int sec.sec_addr ofs in
      let d = Addr.of_int32 (nj_getint32 sec.sec_buf ofs) in
      let x = Addr.add (Addr.sub t r) d in
      nj_putint32 sec.sec_buf ofs (Addr.to_int32 x)

(* Data types *)

let jit_int8 n =
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  grow_section sec (pos + 1);
  String.unsafe_set sec.sec_buf pos (Char.unsafe_chr n)

let jit_int8n n =
  jit_int8 (Nativeint.to_int n)

let jit_int16 n =
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  grow_section sec (pos + 2);
  nj_putint16 sec.sec_buf pos n

let jit_int16n n =
  jit_int16 (Nativeint.to_int n)

let jit_int32l n =
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  grow_section sec (pos + 4);
  nj_putint32 sec.sec_buf pos n

let jit_int32 n =
  jit_int32l (Int32.of_int n)

let jit_int32n n =
  jit_int32l (Nativeint.to_int32 n)

let jit_int64L n =
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  grow_section sec (pos + 8);
  nj_putint64 sec.sec_buf pos n

let jit_int64 n =
  jit_int64L (Int64.of_int n)

let jit_int64n n =
  jit_int64L (Int64.of_nativeint n)

let jit_ascii str =
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  let len = String.length str in
  grow_section sec (pos + len);
  String.unsafe_blit str 0 sec.sec_buf pos len

let jit_asciz s =
  jit_ascii s;
  jit_int8 0

(* Alignment *)

let jit_fill x n = 
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  grow_section sec (pos + n);
  String.unsafe_fill sec.sec_buf pos n (Char.unsafe_chr x)

let jit_align x n =
  let sec = !curr_sec in
  let m = n - (sec.sec_pos mod n) in
  if m != n then
    jit_fill x m

(* Jitting of data *)

let data l =
  jit_data();
  List.iter
     (function
        Cglobal_symbol s ->
          jit_global s
      | Cdefine_symbol s ->
          jit_symbol s
      | Cdefine_label lbl ->
          jit_label (100000 + lbl)
      | Cint8 n ->
          jit_int8 n
      | Cint16 n ->
          jit_int16 n
      | Cint32 n ->
          jit_int32n n
      | Cint n ->
          if Arch.size_addr == 4 then jit_int32n n else jit_int64n n
      | Csingle f ->
          jit_int32l (Int32.bits_of_float (float_of_string f))
      | Cdouble f ->
          jit_int64L (Int64.bits_of_float (float_of_string f))
      | Csymbol_address s ->
          let tag = jit_symbol_tag s in
          if Arch.size_addr == 4 then begin
            jit_reloc (RelocAbs32 tag);
            jit_int32l 0l
          end else begin
            jit_reloc (RelocAbs64 tag);
            jit_int64L 0L
          end
      | Clabel_address lbl ->
          let tag = jit_label_tag (100000 + lbl) in
          if Arch.size_addr == 4 then begin
            jit_reloc (RelocAbs32 tag);
            jit_int32l 0l
          end else begin
            jit_reloc (RelocAbs64 tag);
            jit_int64L 0L
          end
      | Cstring s ->
          jit_ascii s
      | Cskip n ->
          jit_fill 0 n
      | Calign n ->
          jit_align 0 n)
     l

(* Beginning / end of an assembly *)

let begin_assembly() =
  (* Reset JIT state *)
  reset_section text_sec;
  reset_section data_sec;
  labels := [];
  globals := [];
  symbols := [];
  relocs := [];
  (* OCaml module prologue *)
  let sym = (Compilenv.make_symbol (Some "data_begin")) in
  jit_data();
  jit_global sym;
  jit_symbol sym;
  let sym = (Compilenv.make_symbol (Some "code_begin")) in
  jit_text();
  jit_global sym;
  jit_symbol sym

let efa =
  { efa_label = if Arch.size_addr == 4
                then (fun lbl -> jit_reloc (RelocAbs32(jit_label_tag lbl)); jit_int32l 0l)
                else (fun lbl -> jit_reloc (RelocAbs64(jit_label_tag lbl)); jit_int64L 0L);
    efa_16 = jit_int16;
    efa_32 = jit_int32l;
    efa_word = if Arch.size_addr == 4 then jit_int32 else jit_int64;
    efa_align = jit_align 0;
    efa_label_rel = (fun lbl ofs ->
                       (* .long lbl - . + ofs *)
                       jit_reloc (RelocRel32(jit_label_tag lbl));
                       jit_int32l ofs);
    efa_def_label = jit_label;
    efa_string = jit_asciz }

let end_assembly() =
  (* OCaml module epilogue *)
  let sym = (Compilenv.make_symbol (Some "code_end")) in
  jit_text();
  jit_global sym;
  jit_symbol sym;
  let sym = (Compilenv.make_symbol (Some "data_end")) in
  jit_data();
  jit_global sym;
  jit_symbol sym;
  let sym = (Compilenv.make_symbol (Some "frametable")) in
  jit_int32 0;
  jit_global sym;
  jit_symbol sym;
  emit_frames efa;
  (* Allocate memory to sections *)
  let (text, data) = nj_malloc text_sec.sec_pos data_sec.sec_pos in
  text_sec.sec_addr <- text;
  data_sec.sec_addr <- data;
  (* Patch all relocations *)
  List.iter patch_reloc !relocs;
  (* Copy section contents *)
  nj_memcpy text_sec.sec_addr text_sec.sec_buf text_sec.sec_pos;
  nj_memcpy data_sec.sec_addr data_sec.sec_buf data_sec.sec_pos;
  (* Register global symbols *)
  List.iter
    (fun sym -> nj_addsym sym (addr_of_symbol sym))
    !globals
