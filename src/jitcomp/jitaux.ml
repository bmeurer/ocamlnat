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

open Cmm
open Emitaux
open Jitlink
open Linearize

(* Native addressing *)

module Addr =
struct
  include Nativeint

  external of_int64: int64 -> t = "%int64_to_nativeint"
  external to_int64: t -> int64 = "%int64_of_nativeint"

  let add_int x y =
    add x (of_int y)
end

(* String extensions *)

module String =
struct
  include String

  external unsafe_get16: t -> int -> int = "camlnat_str_get16" "noalloc"
  external unsafe_get32: t -> int -> int32 = "camlnat_str_get32"
  external unsafe_get64: t -> int -> int64 = "camlnat_str_get64"
  external unsafe_set16: t -> int -> int -> unit = "camlnat_str_set16" "noalloc"
  external unsafe_set32: t -> int -> int32 -> unit = "camlnat_str_set32" "noalloc"
  external unsafe_set64: t -> int -> int64 -> unit = "camlnat_str_set64" "noalloc"
end

(* Memory management *)

module Memory =
struct
  type t = Addr.t

  external reserve: int -> t = "camlnat_mem_reserve"
  external prepare: t -> string -> int -> unit = "camlnat_mem_prepare" "noalloc"
  external commit: t -> int -> unit = "camlnat_mem_commit"
end

(* Symbol management *)

module Symbol =
struct
  type t = string

  external exec: t -> Obj.t = "camlnat_sym_exec"
  external load: t -> Obj.t = "camlnat_sym_load"
  external add: t -> Addr.t -> unit = "camlnat_sym_add" "noalloc"
  external get: t -> Addr.t = "camlnat_sym_get"
end

(* Execution *)

type evaluation_outcome = Result of Obj.t | Exception of exn

let jit_execsym sym =
  try Result(Symbol.exec sym)
  with exn -> Exception exn

let jit_loadsym sym =
  try Symbol.load sym
  with Failure s -> raise (Error(Undefined_global s))

(* Sections *)

type section =
  { mutable sec_buf: string;
    mutable sec_pos: int;
    mutable sec_addr: Addr.t }

let grow_section sec pos =
  let len = String.length sec.sec_buf in
  if pos > len then begin
    let buf = String.create (len * 2) in
    assert ((String.length buf) mod 1024 == 0);
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
      try Symbol.get sym
      with Failure s -> raise (Error(Undefined_global s))

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
    R_ABS_32 of tag     (* 32bit absolute *)
  | R_ABS_64 of tag     (* 64bit absolute *)
  | R_REL_32 of tag     (* 32bit relative *)
  | R_ARM_JMP_24 of tag (* ARM B/BL offsets *)
  | R_ARM_LDR_12 of tag (* ARM LDR offsets *)

let relocs = ref ([] : (section * int * reloc) list)

let jit_reloc reloc =
  let sec = !curr_sec in
  relocs := (sec, sec.sec_pos, reloc) :: !relocs

let patch_reloc (sec, ofs, rel) =
  match rel with
    R_ABS_32 tag ->
      let a = addr_of_tag tag in
      assert (a >= -2147483648n);
      assert (a <= 2147483647n);
      let d = String.unsafe_get32 sec.sec_buf ofs in
      let x = Int32.add (Addr.to_int32 a) d in
      String.unsafe_set32 sec.sec_buf ofs x
  | R_ABS_64 tag ->
      let a = Addr.to_int64 (addr_of_tag tag) in
      let d = String.unsafe_get64 sec.sec_buf ofs in
      let x = Int64.add a d in
      String.unsafe_set64 sec.sec_buf ofs x
  | R_REL_32 tag ->
      let t = addr_of_tag tag in
      let r = Addr.add_int sec.sec_addr ofs in
      let d = Addr.of_int32 (String.unsafe_get32 sec.sec_buf ofs) in
      let x = Addr.add (Addr.sub t r) d in
      assert (x >= -2147483648n);
      assert (x <= 2147483647n);
      String.unsafe_set32 sec.sec_buf ofs (Addr.to_int32 x)
  | R_ARM_JMP_24 tag -> (* ((S + A) | T) – P *)
      let s = addr_of_tag tag in
      let p = Addr.add_int sec.sec_addr ofs in
      let i = String.unsafe_get32 sec.sec_buf ofs in
      let a = (Int32.to_int i) lsl 2 in
      let a = if (a land 0x2000000) == 0 (* sign extend *)
              then a land 0x3fffffc
              else a lor 0x7c000000 in
      let x = Addr.sub (Addr.add_int s a) p in
      let i = Int32.logor
                (Int32.logand i 0xff000000l)
                (Addr.to_int32 (Addr.logand
                                 (Addr.shift_right x 2)
                                 0xffffffn)) in
      String.unsafe_set32 sec.sec_buf ofs i
  | R_ARM_LDR_12 tag ->
      let s = addr_of_tag tag in
      let p = Addr.add_int sec.sec_addr ofs in
      let i = String.unsafe_get32 sec.sec_buf ofs in
      let a = (Int32.to_int i) land 0xfff in
      let a = if (Int32.logand i 0x800000l) == 0l (* up/down bit *)
              then (-a)
              else a in
      let x = Addr.sub (Addr.add_int s a) p in
      let i = Int32.logor
                (Int32.logand i 0xff7ff000l)
                (Int32.logand
                   (if x < 0n
                    then Addr.to_int32 (Addr.neg x)
                    else (Int32.logor
                            (Addr.to_int32 x)
                            0x800000l))
                   0xfffl) in
      String.unsafe_set32 sec.sec_buf ofs i

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
  String.unsafe_set16 sec.sec_buf pos n

let jit_int16n n =
  jit_int16 (Nativeint.to_int n)

let jit_int32l n =
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  grow_section sec (pos + 4);
  String.unsafe_set32 sec.sec_buf pos n

let jit_int32 n =
  jit_int32l (Int32.of_int n)

let jit_int32n n =
  jit_int32l (Nativeint.to_int32 n)

let jit_int64L n =
  let sec = !curr_sec in
  let pos = sec.sec_pos in
  grow_section sec (pos + 8);
  String.unsafe_set64 sec.sec_buf pos n

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
            jit_reloc (R_ABS_32 tag);
            jit_int32l 0l
          end else begin
            jit_reloc (R_ABS_64 tag);
            jit_int64L 0L
          end
      | Clabel_address lbl ->
          let tag = jit_label_tag (100000 + lbl) in
          if Arch.size_addr == 4 then begin
            jit_reloc (R_ABS_32 tag);
            jit_int32l 0l
          end else begin
            jit_reloc (R_ABS_64 tag);
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
                then (fun lbl ->
                        (* .long lbl *)
                        jit_reloc (R_ABS_32(jit_label_tag lbl));
                        jit_int32l 0l)
                else (fun lbl ->
                        (* .quad lbl *)
                        jit_reloc (R_ABS_64(jit_label_tag lbl));
                        jit_int64L 0L);
    efa_16 = jit_int16;
    efa_32 = jit_int32l;
    efa_word = if Arch.size_addr == 4 then jit_int32 else jit_int64;
    efa_align = jit_align 0;
    efa_label_rel = (fun lbl ofs ->
                       (* .long lbl - . + ofs *)
                       jit_reloc (R_REL_32(jit_label_tag lbl));
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
  (* Pad sections to 64-byte boundaries to avoid having code
     and data on a single (64-byte) cache line (cf. "Software
     Optimization Guide for the AMD64 Processor"). This is
     safe since the section buffer length is always a multiple
     of 1K. *)
  text_sec.sec_pos <- (text_sec.sec_pos + 63) land (-64);
  data_sec.sec_pos <- (data_sec.sec_pos + 63) land (-64);
  assert ((text_sec.sec_pos mod 64) == 0);
  assert ((data_sec.sec_pos mod 64) == 0);
  (* Reserve memory for the sections *)
  text_sec.sec_addr <- Memory.reserve (text_sec.sec_pos + data_sec.sec_pos);
  data_sec.sec_addr <- Addr.add_int text_sec.sec_addr text_sec.sec_pos;
  (* Patch all relocations *)
  List.iter patch_reloc !relocs;
  (* Prepare section content *)
  Memory.prepare text_sec.sec_addr text_sec.sec_buf text_sec.sec_pos;
  Memory.prepare data_sec.sec_addr data_sec.sec_buf data_sec.sec_pos;
  (* Register global symbols *)
  List.iter
    (fun sym -> Symbol.add sym (addr_of_symbol sym))
    !globals;
  (* Commit memory *)
  Memory.commit text_sec.sec_addr (text_sec.sec_pos + data_sec.sec_pos)
