(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                 Marcell Fischbach, University of Siegen             *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,    *)
(*    Universität Siegen. All rights reserved. This file is distri-    *)
(*    buted under the terms of the Q Public License version 1.0.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* JIT emission of Intel 386 assembly code *)

module StringSet = Set.Make(struct type t = string let compare = compare end)

open Location
open Misc
open Cmm
open Arch
open Proc
open Reg
open Mach
open Jitaux
open Emitaux
open Linearize

(* Instructions *)

type argument =
    Register of int
  | Memory of (*ireg*)int * (*scale*)int * (*breg*)int * (*disp*)int
              (* A value of -1 for breg means no base register *)
  | MemoryTag of (*ireg*)int * (*scale*)int * (*disp*)int * (*tag*)tag
                 (* A value of -1 for ireg means absolute addressing *)
  | Immediate of nativeint
  | ImmediateTag of tag

let eax = Register 0
let ecx = Register 1
let esp = Register 4
let ax = eax
let al = ax
let ah = esp

let memscale disp ireg scale =
  match ireg with
    Register ireg ->
      Memory(ireg, scale, -1, disp)
  | _ ->
      assert false

let membase disp breg =
  memscale disp breg 1

let memindex disp breg ireg scale =
  match breg, ireg with
    Register breg, Register ireg ->
      Memory(ireg, scale, breg, disp)
  | _ ->
      assert false

let memtag tag disp =
  MemoryTag(-1, 0, disp, tag)

let memsymbol sym =
  memtag (jit_symbol_tag sym) 0

let memlblscale lbl ireg scale =
  match ireg with
    Register ireg ->
      MemoryTag(ireg, scale, 0, jit_label_tag lbl)
  | _ ->
      assert false

let caml_young_ptr = memsymbol "caml_young_ptr"
let caml_young_limit = memsymbol "caml_young_limit"
let caml_exception_pointer = memsymbol "caml_exception_pointer"

let is_imm8 n = n >= -128 && n <= 127
let is_imm8n n = n >= -128n && n <= 127n

let jit_mod_rm_reg opcodes rm reg =
  let jit_opcodes opcodes =
    if opcodes land 0x00ff00 != 0 then jit_int8 (opcodes asr 8);
    jit_int8 opcodes
  and jit_modrm m rm reg =
    jit_int8 ((m lsl 6) lor rm lor (reg lsl 3))
  and jit_sib s i b =
    let s = (match s with
                1 -> 0
              | 2 -> 1
              | 4 -> 2
              | 8 -> 3
              | _ -> assert false) in
    jit_int8 ((s lsl 6) lor (i lsl 3) lor b)
  in match rm with
  | Register rm ->
      jit_opcodes opcodes;
      jit_modrm 0b11 rm reg
  | Memory(((*%esp*)4) as breg, 1, -1, 0) ->
      jit_opcodes opcodes;
      jit_modrm 0b00 breg reg;
      jit_sib 1 0b100 breg
  | Memory(((*%esp*)4) as breg, 1, -1, disp) when is_imm8 disp ->
      jit_opcodes opcodes;
      jit_modrm 0b01 0b100 reg;
      jit_sib 1 0b100 breg;
      jit_int8 disp
  | Memory(((*%esp*)4) as breg, 1, -1, disp) ->
      jit_opcodes opcodes;
      jit_modrm 0b10 0b100 reg;
      jit_sib 1 0b100 breg;
      jit_int32 disp
  | Memory(((*%ebp*)5) as breg, 1, -1, disp) when is_imm8 disp ->
      jit_opcodes opcodes;
      jit_modrm 0b01 breg reg;
      jit_int8 disp
  | Memory(rm, 1, -1, 0) ->
      jit_opcodes opcodes;
      jit_modrm 0b00 rm reg
  | Memory(rm, 1, -1, disp) when is_imm8 disp ->
      jit_opcodes opcodes;
      jit_modrm 0b01 rm reg;
      jit_int8 disp
  | Memory(rm, 1, -1, disp) ->
      jit_opcodes opcodes;
      jit_modrm 0b10 rm reg;
      jit_int32 disp
  | Memory(ireg, scale, (*no breg*)-1, disp) ->
      jit_opcodes opcodes;
      jit_modrm 0b00 0b100 reg;
      jit_sib scale ireg 0b101;
      jit_int32 disp
  | Memory(ireg, scale, (((*%ebp*)5) as breg), 0) ->
      jit_opcodes opcodes;
      jit_modrm 0b01 0b100 reg;
      jit_sib scale ireg breg;
      jit_int8 0
  | Memory(ireg, scale, breg, 0) ->
      jit_opcodes opcodes;
      jit_modrm 0b00 0b100 reg;
      jit_sib scale ireg breg
  | Memory(ireg, scale, breg, disp) when is_imm8 disp ->
      jit_opcodes opcodes;
      jit_modrm 0b01 0b100 reg;
      jit_sib scale ireg breg;
      jit_int8 disp
  | Memory(ireg, scale, breg, disp) ->
      jit_opcodes opcodes;
      jit_modrm 0b10 0b100 reg;
      jit_sib scale ireg breg;
      jit_int32 disp
  | MemoryTag(-1, _, disp, tag) ->
      (* absolute addressing *)
      jit_opcodes opcodes;
      jit_modrm 0b00 0b101 reg;
      jit_reloc (RelocAbs32 tag);
      jit_int32 disp
  | MemoryTag(ireg, scale, disp, tag) ->
      (* absolute addressing plus scaled index *)
      jit_opcodes opcodes;
      jit_modrm 0b00 0b100 reg;
      jit_sib scale ireg 0b101;
      jit_reloc (RelocAbs32 tag);
      jit_int32 disp
  | _ ->
      assert false

let jit_testl src dst =
  match src, dst with
    Immediate n, Register (*%eax*)0 when is_imm8n n ->
      jit_int8 0xa8;
      jit_int8n n
  | Immediate n, (Register reg as rm) when is_imm8n n && reg < 4 ->
      jit_mod_rm_reg 0xf6 rm 0;
      jit_int8n n
  | Immediate n, Register (*%eax*)0 ->
      jit_int8 0xa9;
      jit_int32n n
  | Immediate n, rm ->
      jit_mod_rm_reg 0xf7 rm 0;
      jit_int32n n
  | Register reg, rm ->
      jit_mod_rm_reg 0x85 rm reg
  | _ ->
      assert false

let jit_movb src dst =
  match src, dst with
    Immediate n, Register reg ->
      jit_int8 (0xb0 + reg);
      jit_int8n n
  | Register reg, rm ->
      jit_mod_rm_reg 0x88 rm reg
  | _ ->
      assert false

let jit_movw src dst =
  jit_int8 0x66;
  match src, dst with
    rm, Register reg ->
      jit_mod_rm_reg 0x8b rm reg
  | Register reg, rm ->
      jit_mod_rm_reg 0x89 rm reg
  | _ ->
      assert false

let jit_movl src dst =
  match src, dst with
    Immediate n, rm ->
      jit_mod_rm_reg 0xc7 rm 0;
      jit_int32n n
  | ImmediateTag tag, rm ->
      jit_mod_rm_reg 0xc7 rm 0;
      jit_reloc (RelocAbs32 tag);
      jit_int32l 0l
  | rm, Register reg ->
      jit_mod_rm_reg 0x8b rm reg
  | Register reg, rm ->
      jit_mod_rm_reg 0x89 rm reg
  | _ ->
      assert false

let jit_rmreg opcode src dst =
  match src, dst with
    rm, Register reg ->
      jit_mod_rm_reg opcode rm reg
  | _ ->
      assert false

let jit_movsbl src dst = jit_rmreg 0x0fbe src dst
let jit_movswl src dst = jit_rmreg 0x0fbf src dst
let jit_movzbl src dst = jit_rmreg 0x0fb6 src dst
let jit_movzwl src dst = jit_rmreg 0x0fb7 src dst
let jit_leal   src dst = jit_rmreg 0x008d src dst

let jit_alub op src dst =
  match src, dst with
    Immediate n, Register (*%eax*)0 ->
      jit_int8 ((op lsl 3) + 4);
      jit_int8n n
  | Immediate n, rm ->
      jit_mod_rm_reg 0x80 rm op;
      jit_int8n n
  | _ ->
      assert false

let jit_andb src dst = jit_alub 4 src dst
let jit_subb src dst = jit_alub 5 src dst
let jit_xorb src dst = jit_alub 6 src dst
let jit_cmpb src dst = jit_alub 7 src dst

let jit_alul op src dst =
  match src, dst with
    Immediate n, rm when is_imm8n n ->
      jit_mod_rm_reg 0x83 rm op;
      jit_int8n n
  | Immediate n, Register (*%eax*)0 ->
      jit_int8 ((op lsl 3) + 5);
      jit_int32n n
  | Immediate n, rm ->
      jit_mod_rm_reg 0x81 rm op;
      jit_int32n n
  | rm, Register reg ->
      jit_mod_rm_reg ((op lsl 3) + 3) rm reg
  | Register reg, rm ->
      jit_mod_rm_reg ((op lsl 3) + 1) rm reg
  | _ ->
      assert false

let jit_addl src dst = jit_alul 0 src dst
let jit_orl  src dst = jit_alul 1 src dst
let jit_andl src dst = jit_alul 4 src dst
let jit_subl src dst = jit_alul 5 src dst
let jit_xorl src dst = jit_alul 6 src dst
let jit_cmpl src dst = jit_alul 7 src dst

let jit_imull src dst =
  match src, dst with
    Immediate n, (Register reg as rm) when is_imm8n n ->
      jit_mod_rm_reg 0x6b rm reg;
      jit_int8n n
  | Immediate n, (Register reg as rm) ->
      jit_mod_rm_reg 0x69 rm reg;
      jit_int32n n
  | rm, Register reg ->
      jit_mod_rm_reg 0x0faf rm reg
  | _ ->
      assert false

let jit_idivl dst =
  jit_mod_rm_reg 0xf7 dst 7

let jit_shfl op src dst =
  match src, dst with
    Immediate 1n, rm ->
      jit_mod_rm_reg 0xd1 rm op
  | Immediate n, rm ->
      jit_mod_rm_reg 0xc1 rm op;
      jit_int8n n
  | (*%cl*)Register 1, rm ->
      jit_mod_rm_reg 0xd3 rm op
  | _ ->
      assert false

let jit_sall src dst = jit_shfl 4 src dst
let jit_shrl src dst = jit_shfl 5 src dst
let jit_sarl src dst = jit_shfl 7 src dst

let jit_jmpl dst =
  jit_mod_rm_reg 0xff dst 4

let jit_jmp_tag tag =
  jit_int8 0xe9;
  jit_reloc (RelocRel32 tag);
  jit_int32l (-4l)

let jit_jmp_label lbl =
  jit_jmp_tag (jit_label_tag lbl)

let jit_jmp_symbol sym =
  jit_jmp_tag (jit_symbol_tag sym)

let jit_calll dst =
  jit_mod_rm_reg 0xff dst 2

let jit_call_tag tag =
  jit_int8 0xe8;
  jit_reloc (RelocRel32 tag);
  jit_int32l (-4l)

let jit_call_label lbl =
  jit_call_tag (jit_label_tag lbl)

let jit_call_symbol sym =
  jit_call_tag (jit_symbol_tag sym)

let jit_ret() =
  jit_int8 0xc3

let jit_pushl = function
    Immediate n when is_imm8n n ->
      jit_int8 0x6a;
      jit_int8n n
  | Immediate n ->
      jit_int8 0x68;
      jit_int32n n
  | ImmediateTag tag ->
      jit_int8 0x68;
      jit_reloc (RelocAbs32 tag);
      jit_int32l 0l
  | Register reg ->
      jit_int8 (0x50 + reg)
  | rm ->
      jit_mod_rm_reg 0xff rm 6

let jit_popl = function
    Register reg ->
      jit_int8 (0x58 + reg)
  | rm ->
      jit_mod_rm_reg 0x8f rm 0

let jit_cltd() =
  jit_int8 0x99

(* Condition codes *)
type cc =
    O   | NO  | B   | NB
  | Z   | NZ  | BE  | NBE
  | S   | NS  | P   | NP
  | L   | NL  | LE  | NLE

external int_of_cc: cc -> int = "%identity"

let jit_jcc_label cc lbl =
  jit_int8 0x0f;
  jit_int8 (0x80 + (int_of_cc cc));
  jit_reloc (RelocRel32(jit_label_tag lbl));
  jit_int32l (-4l)

let jit_jb_label   lbl = jit_jcc_label B   lbl
let jit_jnb_label  lbl = jit_jcc_label NB  lbl
let jit_jz_label   lbl = jit_jcc_label Z   lbl
let jit_jnz_label  lbl = jit_jcc_label NZ  lbl
let jit_jbe_label  lbl = jit_jcc_label BE  lbl
let jit_jnbe_label lbl = jit_jcc_label NBE lbl
let jit_jp_label   lbl = jit_jcc_label P   lbl
let jit_jnp_label  lbl = jit_jcc_label NP  lbl
let jit_jl_label   lbl = jit_jcc_label L   lbl
let jit_jnl_label  lbl = jit_jcc_label NL  lbl
let jit_jle_label  lbl = jit_jcc_label LE  lbl
let jit_jnle_label lbl = jit_jcc_label NLE lbl

let jit_setcc cc dst =
  jit_mod_rm_reg (0x0f90 + (int_of_cc cc)) dst 0


(* FPU Instructions *)

let jit_fadds  src = jit_mod_rm_reg 0xd8 src 0
let jit_fmuls  src = jit_mod_rm_reg 0xd8 src 1
let jit_fsubs  src = jit_mod_rm_reg 0xd8 src 4
let jit_fsubrs src = jit_mod_rm_reg 0xd8 src 5
let jit_fdivs  src = jit_mod_rm_reg 0xd8 src 6
let jit_fdivrs src = jit_mod_rm_reg 0xd8 src 7

let jit_fpu_d9 op =
  jit_int8 0xd9;
  jit_int8 op

let jit_fxch()   = jit_fpu_d9 0xc9
let jit_fchs()   = jit_fpu_d9 0xe0
let jit_fabs()   = jit_fpu_d9 0xe1
let jit_fld1()   = jit_fpu_d9 0xe8
let jit_fldlg2() = jit_fpu_d9 0xec
let jit_fldln2() = jit_fpu_d9 0xed
let jit_fldz()   = jit_fpu_d9 0xee
let jit_fyl2x()  = jit_fpu_d9 0xf1
let jit_fptan()  = jit_fpu_d9 0xf2
let jit_fpatan() = jit_fpu_d9 0xf3
let jit_fsqrt()  = jit_fpu_d9 0xfa
let jit_fsin()   = jit_fpu_d9 0xfe
let jit_fcos()   = jit_fpu_d9 0xff

let jit_flds   src = jit_mod_rm_reg 0xd9 src 0
let jit_fstps  dst = jit_mod_rm_reg 0xd9 dst 3
let jit_fldcw  src = jit_mod_rm_reg 0xd9 src 5
let jit_fnstcw dst = jit_mod_rm_reg 0xd9 dst 7
let jit_fildl  src = jit_mod_rm_reg 0xdb src 0
let jit_fistpl dst = jit_mod_rm_reg 0xdb dst 3
let jit_faddl  src = jit_mod_rm_reg 0xdc src 0
let jit_fmull  src = jit_mod_rm_reg 0xdc src 1
let jit_fcompl src = jit_mod_rm_reg 0xdc src 3
let jit_fsubl  src = jit_mod_rm_reg 0xdc src 4
let jit_fsubrl src = jit_mod_rm_reg 0xdc src 5
let jit_fdivl  src = jit_mod_rm_reg 0xdc src 6
let jit_fdivrl src = jit_mod_rm_reg 0xdc src 7
let jit_fldl   src = jit_mod_rm_reg 0xdd src 0
let jit_fstpl  dst = jit_mod_rm_reg 0xdd dst 3

let jit_fstp() =
  jit_int8 0xdd;
  jit_int8 0xd8

let jit_fpu_de op =
  jit_int8 0xde;
  jit_int8 op

let jit_faddp()  = jit_fpu_de 0xc1
let jit_fmulp()  = jit_fpu_de 0xc9
let jit_fcompp() = jit_fpu_de 0xd9
let jit_fsubrp() = jit_fpu_de 0xe1
let jit_fsubp()  = jit_fpu_de 0xe9
let jit_fdivrp() = jit_fpu_de 0xf1
let jit_fdivp()  = jit_fpu_de 0xf9

let jit_fnstsw = function
    (*%ax*)Register 0 ->
      jit_int8 0xdf;
      jit_int8 0xe0
  | _ ->
      assert false

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

let stack_offset = ref 0

(* Layout of the stack frame *)

let frame_size() =                     (* includes return address *)
  let sz =
    !stack_offset + 4 * num_stack_slots.(0) + 8 * num_stack_slots.(1) + 4
  in Misc.align sz stack_alignment

let slot_offset loc cl =
  match loc with
    Incoming n ->
      assert (n >= 0);
      frame_size() + n
  | Local n ->
      if cl = 0
      then !stack_offset + n * 4
      else !stack_offset + num_stack_slots.(0) * 4 + n * 8
  | Outgoing n ->
      assert (n >= 0);
      n

let trap_frame_size = Misc.align 8 stack_alignment

(* Output a pseudo-register *)

(* Map register index to phys index (cf. proc.ml) *)
let int_reg_index =
  [| (*%eax*)0; (*%ebx*)3; (*%ecx*)1; (*%edx*)2;
     (*%esi*)6; (*%edi*)7; (*%ebp*)5 |]

let register_index r =
  int_reg_index.(r)

let caml_extra_params = jit_symbol_tag "caml_extra_params"

let emit_reg = function
    { loc = Reg r } ->
      Register(register_index r)
  | { loc = Stack(Incoming n | Outgoing n) } when n < 0 ->
      memtag caml_extra_params (n + 64)
  | { loc = Stack s } as r ->
      let ofs = slot_offset s (register_class r) in
      membase ofs esp
  | { loc = Unknown } ->
      fatal_error "Jit_i386.emit_reg"

(* Output an addressing mode *)

let emit_addressing addr r n =
  match addr with
    Ibased(s, d) ->
      memtag (jit_symbol_tag s) d
  | Iindexed d ->
      membase d (emit_reg r.(n))
  | Iindexed2 d ->
      memindex d (emit_reg r.(n)) (emit_reg r.(n+1)) 1
  | Iscaled(2, d) ->
      memindex d (emit_reg r.(n)) (emit_reg r.(n)) 1
  | Iscaled(scale, d) ->
      memscale d (emit_reg r.(n)) scale
  | Iindexed2scaled(scale, d) ->
      memindex d (emit_reg r.(n)) (emit_reg r.(n+1)) scale

(* Record live pointers at call points *)

let record_frame_label live dbg =
  let lbl = new_label() in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
        {typ = Addr; loc = Reg r} ->
          live_offset := ((r lsl 1) + 1) :: !live_offset
      | {typ = Addr; loc = Stack s} as reg ->
          live_offset := slot_offset s (register_class reg) :: !live_offset
      | _ -> ())
    live;
  frame_descriptors :=
    { fd_lbl = lbl;
      fd_frame_size = frame_size();
      fd_live_offset = !live_offset;
      fd_debuginfo = dbg } :: !frame_descriptors;
  lbl

let record_frame live dbg =
  let lbl = record_frame_label live dbg in jit_label lbl

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: label;                      (* Entry label *)
    gc_return_lbl: label;               (* Where to branch after GC *)
    gc_frame: label }                   (* Label of frame descriptor *)

let call_gc_sites = ref ([] : gc_call list)

let emit_call_gc gc =
  jit_label gc.gc_lbl;
  jit_call_symbol "caml_call_gc";
  jit_label gc.gc_frame;
  jit_jmp_label gc.gc_return_lbl

(* Record calls to caml_ml_array_bound_error. *)

let bound_error_call = ref 0

let bound_error_label dbg =
 if !bound_error_call = 0 then bound_error_call := new_label();
 !bound_error_call

let emit_call_bound_errors() =
  if !bound_error_call > 0 then begin
    jit_label !bound_error_call;
    jit_call_symbol "caml_ml_array_bound_error"
  end

(* Names for instructions *)

let instr_for_intop = function
    Iadd -> jit_addl
  | Isub -> jit_subl
  | Imul -> jit_imull
  | Iand -> jit_andl
  | Ior -> jit_orl
  | Ixor -> jit_xorl
  | Ilsl -> jit_sall
  | Ilsr -> jit_shrl
  | Iasr -> jit_sarl
  | _ -> fatal_error "Jit_i386: instr_for_intop"

let instr_for_floatop = function
    Iaddf -> jit_faddl
  | Isubf -> jit_fsubl
  | Imulf -> jit_fmull
  | Idivf -> jit_fdivl
  | Ispecific Isubfrev -> jit_fsubrl
  | Ispecific Idivfrev -> jit_fdivrl
  | _ -> fatal_error "Jit_i386: instr_for_floatop"

let instr_for_floatop_reversed = function
    Iaddf -> jit_faddl
  | Isubf -> jit_fsubrl
  | Imulf -> jit_fmull
  | Idivf -> jit_fdivrl
  | Ispecific Isubfrev -> jit_fsubl
  | Ispecific Idivfrev -> jit_fdivl
  | _ -> fatal_error "Jit_i386: instr_for_floatop_reversed"

let instr_for_floatop_pop = function
    Iaddf -> jit_faddp()
  | Isubf -> jit_fsubrp()
  | Imulf -> jit_fmulp()
  | Idivf -> jit_fdivrp()
  | Ispecific Isubfrev -> jit_fsubp()
  | Ispecific Idivfrev -> jit_fdivp()
  | _ -> fatal_error "Jit_i386: instr_for_floatop_pop"

let instr_for_floatarithmem double = function
    Ifloatadd -> if double then jit_faddl else jit_fadds
  | Ifloatsub -> if double then jit_fsubl else jit_fsubs
  | Ifloatsubrev -> if double then jit_fsubrl else jit_fsubrs
  | Ifloatmul -> if double then jit_fmull else jit_fmuls
  | Ifloatdiv -> if double then jit_fdivl else jit_fdivs
  | Ifloatdivrev -> if double then jit_fdivrl else jit_fdivrs

let cc_for_cond_branch = function
    Isigned   Ceq -> Z  | Isigned   Cne -> NZ
  | Isigned   Cle -> LE | Isigned   Cgt -> NLE
  | Isigned   Clt -> L  | Isigned   Cge -> NL
  | Iunsigned Ceq -> Z  | Iunsigned Cne -> NZ
  | Iunsigned Cle -> BE | Iunsigned Cgt -> NBE
  | Iunsigned Clt -> B  | Iunsigned Cge -> NB

(* Output an = 0 or <> 0 test. *)

let output_test_zero arg =
  match arg.loc with
    Reg r -> jit_testl (emit_reg arg) (emit_reg arg)
  | _     -> jit_cmpl (Immediate 0n) (emit_reg arg)

(* Deallocate the stack frame before a return or tail call *)

let output_epilogue() =
  let n = frame_size() - 4 in
  if n > 0 then jit_addl (Immediate (Nativeint.of_int n)) esp

(* Determine if the given register is the top of the floating-point stack *)

let is_tos = function { loc = Reg _; typ = Float } -> true | _ -> false

(* Emit the code for a floating-point comparison *)

let emit_float_test cmp neg arg lbl =
  let actual_cmp =
    match (is_tos arg.(0), is_tos arg.(1)) with
      (true, true) ->
      (* both args on top of FP stack *)
      jit_fcompp();
      cmp
    | (true, false) ->
      (* first arg on top of FP stack *)
      jit_fcompl (emit_reg arg.(1));
      cmp
    | (false, true) ->
      (* second arg on top of FP stack *)
      jit_fcompl (emit_reg arg.(0));
      Cmm.swap_comparison cmp
    | (false, false) ->
      jit_fldl (emit_reg arg.(0));
      jit_fcompl (emit_reg arg.(1));
      cmp
    in
  jit_fnstsw ax;
  (begin match actual_cmp with
    Ceq ->
      if neg then begin
      jit_andb (Immediate 68n) ah;
      jit_xorb (Immediate 64n) ah;
      jit_jnz_label
      end else begin
      jit_andb (Immediate 69n) ah;
      jit_cmpb (Immediate 64n) ah;
      jit_jz_label
      end
  | Cne ->
      if neg then begin
      jit_andb (Immediate 69n) ah;
      jit_cmpb (Immediate 64n) ah;
      jit_jz_label
      end else begin
      jit_andb (Immediate 68n) ah;
      jit_xorb (Immediate 64n) ah;
      jit_jnz_label
      end
  | Cle ->
      jit_andb (Immediate 69n) ah;
      jit_subb (Immediate 1n) ah;
      jit_cmpb (Immediate 64n) ah;
      if neg
      then jit_jnb_label
      else jit_jb_label
  | Cge ->
      jit_andb (Immediate 5n) ah;
      if neg
      then jit_jnz_label
      else jit_jz_label
  | Clt ->
      jit_andb (Immediate 69n) ah;
      jit_cmpb (Immediate 1n) ah;
      if neg
      then jit_jnz_label
      else jit_jz_label
  | Cgt ->
      jit_andb (Immediate 69n) ah;
      if neg
      then jit_jnz_label
      else jit_jz_label
  end) lbl

(* Emit a Ifloatspecial instruction *)

let emit_floatspecial = function
    "atan"  -> jit_fld1(); jit_fpatan()
  | "atan2" -> jit_fpatan()
  | "cos"   -> jit_fcos()
  | "log"   -> jit_fldln2(); jit_fxch(); jit_fyl2x()
  | "log10" -> jit_fldlg2(); jit_fxch(); jit_fyl2x()
  | "sin"   -> jit_fsin()
  | "sqrt"  -> jit_fsqrt()
  | "tan"   -> jit_fptan(); jit_fstp()
  | _ -> assert false

(* Output the assembly code for an instruction *)

(* Name of current function *)
let function_name = ref ""
(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref 0
(* Label of trap for out-of-range accesses *)
let range_check_trap = ref 0
(* Record float literals to be emitted later *)
let float_constants = ref ([] : (int * string) list)

let emit_instr fallthrough i =
    match i.desc with
      Lend -> ()
    | Lop(Imove | Ispill | Ireload) ->
        let src = i.arg.(0) and dst = i.res.(0) in
        if src.loc <> dst.loc then begin
          if src.typ = Float then
            if is_tos src then
              jit_fstpl (emit_reg dst)
            else if is_tos dst then
              jit_fldl (emit_reg src)
            else begin
              jit_fldl (emit_reg src);
              jit_fstpl (emit_reg dst)
            end
          else
              jit_movl (emit_reg src) (emit_reg dst)
        end
    | Lop(Iconst_int n) ->
        begin match n, i.res.(0).loc with
          0n, Reg _ ->
            jit_xorl (emit_reg i.res.(0)) (emit_reg i.res.(0))
        | n, _ ->
            jit_movl (Immediate n) (emit_reg i.res.(0))
        end
    | Lop(Iconst_float s) ->
        begin match Int64.bits_of_float (float_of_string s) with
        | 0x0000_0000_0000_0000L ->       (* +0.0 *)
          jit_fldz()
        | 0x8000_0000_0000_0000L ->       (* -0.0 *)
          jit_fldz(); jit_fchs()
        | 0x3FF0_0000_0000_0000L ->       (*  1.0 *)
          jit_fld1()
        | 0xBFF0_0000_0000_0000L ->       (* -1.0 *)
          jit_fld1(); jit_fchs()
        | _ ->
          let lbl = new_label() in
          float_constants := (lbl, s) :: !float_constants;
          jit_fldl (memtag (jit_label_tag lbl) 0)
        end
    | Lop(Iconst_symbol s) ->
        jit_movl (ImmediateTag(jit_symbol_tag s)) (emit_reg i.res.(0))
    | Lop(Icall_ind) ->
        jit_calll (emit_reg i.arg.(0));
        record_frame i.live i.dbg
    | Lop(Icall_imm s) ->
        jit_call_symbol s;
        record_frame i.live i.dbg
    | Lop(Itailcall_ind) ->
        output_epilogue();
        jit_jmpl (emit_reg i.arg.(0))
    | Lop(Itailcall_imm s) ->
        if s = !function_name then
          jit_jmp_label !tailrec_entry_point
        else begin
          output_epilogue();
          jit_jmp_symbol s
        end
    | Lop(Iextcall(s, alloc)) ->
        if alloc then begin
          jit_movl (ImmediateTag(jit_symbol_tag s)) eax;
          jit_call_symbol "caml_c_call";
          record_frame i.live i.dbg
        end else begin
          jit_call_symbol s
        end
    | Lop(Istackoffset n) ->
        jit_subl (Immediate (Nativeint.of_int n)) esp;
        stack_offset := !stack_offset + n
    | Lop(Iload(chunk, addr)) ->
        let dest = i.res.(0) in
        begin match chunk with
          | Word | Thirtytwo_signed | Thirtytwo_unsigned ->
              jit_movl (emit_addressing addr i.arg 0) (emit_reg dest)
          | Byte_unsigned ->
              jit_movzbl (emit_addressing addr i.arg 0) (emit_reg dest)
          | Byte_signed ->
              jit_movsbl (emit_addressing addr i.arg 0) (emit_reg dest)
          | Sixteen_unsigned ->
              jit_movzwl (emit_addressing addr i.arg 0) (emit_reg dest)
          | Sixteen_signed ->
              jit_movswl (emit_addressing addr i.arg 0) (emit_reg dest)
          | Single ->
            jit_flds (emit_addressing addr i.arg 0)
          | Double | Double_u ->
            jit_fldl (emit_addressing addr i.arg 0)
        end
    | Lop(Istore(chunk, addr)) ->
        begin match chunk with
          | Word | Thirtytwo_signed | Thirtytwo_unsigned ->
            jit_movl (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
          | Byte_unsigned | Byte_signed ->
            jit_movb (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
          | Sixteen_unsigned | Sixteen_signed ->
            jit_movw (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
          | Single ->
              if is_tos i.arg.(0) then
                jit_fstps (emit_addressing addr i.arg 1)
              else begin
                jit_fldl (emit_reg i.arg.(0));
                jit_fstps (emit_addressing addr i.arg 1)
              end
          | Double | Double_u ->
              if is_tos i.arg.(0) then
                jit_fstpl (emit_addressing addr i.arg 1)
              else begin
                jit_fldl (emit_reg i.arg.(0));
                jit_fstpl (emit_addressing addr i.arg 1)
              end
        end
    | Lop(Ialloc n) ->
        if !fastcode_flag then begin
          let lbl_redo = new_label() in
          jit_label lbl_redo;
          jit_movl caml_young_ptr eax;
          jit_subl (Immediate (Nativeint.of_int n)) eax;
          jit_movl eax caml_young_ptr;
          jit_cmpl caml_young_limit eax;
          let lbl_call_gc = new_label() in
          let lbl_frame = record_frame_label i.live Debuginfo.none in
          jit_jb_label lbl_call_gc;
          jit_leal (membase 4 eax) (emit_reg i.res.(0));
          call_gc_sites :=
            { gc_lbl = lbl_call_gc;
              gc_return_lbl = lbl_redo;
              gc_frame = lbl_frame } :: !call_gc_sites
        end else begin
          begin match n with
            8  -> jit_call_symbol "caml_alloc1"
          | 12 -> jit_call_symbol "caml_alloc2"
          | 16 -> jit_call_symbol "caml_alloc3"
          | _  -> jit_movl (Immediate (Nativeint.of_int n)) eax;
                  jit_call_symbol "caml_allocN"
          end;
          record_frame i.live Debuginfo.none;
          jit_leal (membase 4 eax) (emit_reg i.res.(0))
        end
    | Lop(Iintop(Icomp cmp)) ->
        jit_cmpl (emit_reg i.arg.(1)) (emit_reg i.arg.(0));
        let cc = cc_for_cond_branch cmp in
        jit_setcc cc al;
        jit_movzbl al (emit_reg i.res.(0))
    | Lop(Iintop_imm(Icomp cmp, n)) ->
        jit_cmpl (Immediate (Nativeint.of_int n)) (emit_reg i.arg.(0));
        let cc = cc_for_cond_branch cmp in
        jit_setcc cc al;
        jit_movzbl al (emit_reg i.res.(0))
    | Lop(Iintop Icheckbound) ->
        let lbl = bound_error_label i.dbg in
        jit_cmpl (emit_reg i.arg.(1)) (emit_reg i.arg.(0));
        jit_jbe_label lbl
    | Lop(Iintop_imm(Icheckbound, n)) ->
        let lbl = bound_error_label i.dbg in
        jit_cmpl (Immediate (Nativeint.of_int n)) (emit_reg i.arg.(0));
        jit_jbe_label lbl
    | Lop(Iintop(Idiv | Imod)) ->
        jit_cltd();
        jit_idivl (emit_reg i.arg.(1))
    | Lop(Iintop(Ilsl | Ilsr | Iasr as op)) ->
        (* We have i.arg.(0) = i.res.(0) and i.arg.(1) = %ecx *)
        (instr_for_intop op) ecx (emit_reg i.res.(0))
    | Lop(Iintop op) ->
        (* We have i.arg.(0) = i.res.(0) *)
        (instr_for_intop op) (emit_reg i.arg.(1)) (emit_reg i.res.(0))
    | Lop(Iintop_imm(Iadd, n)) when i.arg.(0).loc <> i.res.(0).loc ->
        jit_leal (membase n (emit_reg i.arg.(0))) (emit_reg i.res.(0))
    | Lop(Iintop_imm(Idiv, n)) ->
        let l = Misc.log2 n in
        let lbl = new_label() in
        output_test_zero i.arg.(0);
        jit_jnl_label lbl;
        jit_addl (Immediate (Nativeint.of_int (n-1))) (emit_reg i.arg.(0));
        jit_label lbl;
        jit_sarl (Immediate (Nativeint.of_int l)) (emit_reg i.arg.(0))
    | Lop(Iintop_imm(Imod, n)) ->
        let lbl = new_label() in
        jit_movl (emit_reg i.arg.(0)) eax;
        jit_testl eax eax;
        jit_jnl_label lbl;
        jit_addl (Immediate (Nativeint.of_int (n-1))) eax;
        jit_label lbl;
        jit_andl (Immediate (Nativeint.of_int (-n))) eax;
        jit_subl eax (emit_reg i.arg.(0))
    | Lop(Iintop_imm(op, n)) ->
        (* We have i.arg.(0) = i.res.(0) *)
        (instr_for_intop op) (Immediate (Nativeint.of_int n)) (emit_reg i.res.(0))
    | Lop(Iabsf) ->
        if not (is_tos i.arg.(0)) then
          jit_fldl (emit_reg i.arg.(0));
        jit_fabs()
    | Lop(Inegf) ->
        if not (is_tos i.arg.(0)) then
          jit_fldl (emit_reg i.arg.(0));
        jit_fchs()
    | Lop(Iaddf | Isubf | Imulf | Idivf | Ispecific(Isubfrev | Idivfrev)
          as floatop) ->
        begin match (is_tos i.arg.(0), is_tos i.arg.(1)) with
          (true, true) ->
          (* both operands on top of FP stack *)
          instr_for_floatop_pop floatop
        | (true, false) ->
          (* first operand on stack *)
          (instr_for_floatop floatop) (emit_reg i.arg.(1))
        | (false, true) ->
          (* second operand on stack *)
          (instr_for_floatop_reversed floatop) (emit_reg i.arg.(0))
        | (false, false) ->
          (* both operands in memory *)
          jit_fldl (emit_reg i.arg.(0));
          (instr_for_floatop floatop) (emit_reg i.arg.(1))
        end
    | Lop(Ifloatofint) ->
        begin match i.arg.(0).loc with
          Stack s ->
            jit_fildl (emit_reg i.arg.(0))
        | _ ->
            jit_pushl (emit_reg i.arg.(0));
            jit_fildl (membase 0 esp);
            jit_addl (Immediate 4n) esp
        end
    | Lop(Iintoffloat) ->
        if not (is_tos i.arg.(0)) then
          jit_fldl (emit_reg i.arg.(0));
        stack_offset := !stack_offset - 8;
        jit_subl (Immediate 8n) esp;
        jit_fnstcw (membase 4 esp);
        jit_movw (membase 4 esp) ax;
        jit_movb (Immediate 12n) ah;
        jit_movw ax (membase 0 esp);
        jit_fldcw (membase 0 esp);
        begin match i.res.(0).loc with
          Stack s ->
            jit_fistpl (emit_reg i.res.(0))
        | _ ->
            jit_fistpl (membase 0 esp);
            jit_movl (membase 0 esp) (emit_reg i.res.(0))
        end;
        jit_fldcw (membase 4 esp);
        jit_addl (Immediate 8n) esp;
        stack_offset := !stack_offset + 8
    | Lop(Ispecific(Ilea addr)) ->
        jit_leal (emit_addressing addr i.arg 0) (emit_reg i.res.(0))
    | Lop(Ispecific(Istore_int(n, addr))) ->
        jit_movl (Immediate n) (emit_addressing addr i.arg 0)
    | Lop(Ispecific(Istore_symbol(s, addr))) ->
        jit_movl (ImmediateTag(jit_symbol_tag s)) (emit_addressing addr i.arg 0)
    | Lop(Ispecific(Ioffset_loc(n, addr))) ->
        jit_addl (Immediate (Nativeint.of_int n)) (emit_addressing addr i.arg 0)
    | Lop(Ispecific(Ipush)) ->
        (* Push arguments in reverse order *)
        for n = Array.length i.arg - 1 downto 0 do
          let r = i.arg.(n) in
          match r with
            {loc = Reg _; typ = Float} ->
              jit_subl (Immediate 8n) esp;
              jit_fstpl (membase 0 esp);
              stack_offset := !stack_offset + 8
          | {loc = Stack sl; typ = Float} ->
              let ofs = slot_offset sl 1 in
              jit_pushl (membase (ofs + 4) esp);
              jit_pushl (membase (ofs + 4) esp);
              stack_offset := !stack_offset + 8
          | _ ->
              jit_pushl (emit_reg r);
              stack_offset := !stack_offset + 4
        done
    | Lop(Ispecific(Ipush_int n)) ->
        jit_pushl (Immediate n);
        stack_offset := !stack_offset + 4
    | Lop(Ispecific(Ipush_symbol s)) ->
        jit_pushl (ImmediateTag(jit_symbol_tag s));
        stack_offset := !stack_offset + 4
    | Lop(Ispecific(Ipush_load addr)) ->
        jit_pushl (emit_addressing addr i.arg 0);
        stack_offset := !stack_offset + 4
    | Lop(Ispecific(Ipush_load_float addr)) ->
        jit_pushl (emit_addressing (offset_addressing addr 4) i.arg 0);
        jit_pushl (emit_addressing addr i.arg 0);
        stack_offset := !stack_offset + 8
    | Lop(Ispecific(Ifloatarithmem(double, op, addr))) ->
        if not (is_tos i.arg.(0)) then
          jit_fldl (emit_reg i.arg.(0));
        (instr_for_floatarithmem double op) (emit_addressing addr i.arg 1)
    | Lop(Ispecific(Ifloatspecial s)) ->
        (* Push args on float stack if necessary *)
        for k = 0 to Array.length i.arg - 1 do
          if not (is_tos i.arg.(k)) then jit_fldl (emit_reg i.arg.(k))
        done;
        (* Fix-up for binary instrs whose args were swapped *)
        if Array.length i.arg = 2 && is_tos i.arg.(1) then
          jit_fxch();
        emit_floatspecial s
    | Lreloadretaddr ->
        ()
    | Lreturn ->
        output_epilogue();
        jit_ret()
    | Llabel lbl ->
        if not fallthrough && !fastcode_flag then jit_align 0x90 16;
        jit_label lbl
    | Lbranch lbl ->
        jit_jmp_label lbl
    | Lcondbranch(tst, lbl) ->
        begin match tst with
          Itruetest ->
            output_test_zero i.arg.(0);
            jit_jnz_label lbl
        | Ifalsetest ->
            output_test_zero i.arg.(0);
            jit_jz_label lbl
        | Iinttest cmp ->
            jit_cmpl (emit_reg i.arg.(1)) (emit_reg i.arg.(0));
            let cc = cc_for_cond_branch cmp in
            jit_jcc_label cc lbl
        | Iinttest_imm((Isigned Ceq | Isigned Cne |
                        Iunsigned Ceq | Iunsigned Cne) as cmp, 0) ->
            output_test_zero i.arg.(0);
            let cc = cc_for_cond_branch cmp in
            jit_jcc_label cc lbl
        | Iinttest_imm(cmp, n) ->
            jit_cmpl (Immediate (Nativeint.of_int n)) (emit_reg i.arg.(0));
            let cc = cc_for_cond_branch cmp in
            jit_jcc_label cc lbl
        | Ifloattest(cmp, neg) ->
            emit_float_test cmp neg i.arg lbl
        | Ioddtest ->
            jit_testl (Immediate 1n) (emit_reg i.arg.(0));
            jit_jnz_label lbl
        | Ieventest ->
            jit_testl (Immediate 1n) (emit_reg i.arg.(0));
            jit_jz_label lbl
        end
    | Lcondbranch3(lbl0, lbl1, lbl2) ->
            jit_cmpl (Immediate 1n) (emit_reg i.arg.(0));
            begin match lbl0 with
              None -> ()
            | Some lbl -> jit_jb_label lbl
            end;
            begin match lbl1 with
              None -> ()
            | Some lbl -> jit_jz_label lbl
            end;
            begin match lbl2 with
              None -> ()
            | Some lbl -> jit_jnle_label lbl
            end
    | Lswitch jumptbl ->
        let lbl = new_label() in
        jit_jmpl (memlblscale lbl (emit_reg i.arg.(0)) 4);
        jit_data();
        jit_align 0 4;
        jit_label lbl;
        for i = 0 to Array.length jumptbl - 1 do
          jit_reloc (RelocAbs32(jit_label_tag jumptbl.(i)));
          jit_int32l 0l
        done;
        jit_text()
    | Lsetuptrap lbl ->
        jit_call_label lbl
    | Lpushtrap ->
        if trap_frame_size > 8 then
          jit_subl (Immediate (Nativeint.of_int (trap_frame_size - 8))) esp;
        jit_pushl caml_exception_pointer;
        jit_movl esp caml_exception_pointer;
        stack_offset := !stack_offset + trap_frame_size
    | Lpoptrap ->
        jit_popl caml_exception_pointer;
        jit_addl (Immediate (Nativeint.of_int (trap_frame_size - 4))) esp;
        stack_offset := !stack_offset - trap_frame_size
    | Lraise ->
        jit_movl caml_exception_pointer esp;
        jit_popl caml_exception_pointer;
        if trap_frame_size > 8 then
          jit_addl (Immediate (Nativeint.of_int (trap_frame_size - 8))) esp;
        jit_ret()

let rec emit_all fallthrough i =
  match i.desc with
  |  Lend -> ()
  | _ ->
      emit_instr fallthrough i;
      emit_all
        (Linearize.has_fallthrough  i.desc)
        i.next

(* Emission of the floating-point constants *)

let emit_float_constant (lbl, cst) =
  jit_label lbl;
  jit_int64L (Int64.bits_of_float (float_of_string cst))

(* Emission of a function declaration *)

let fundecl fundecl =
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := new_label();
  stack_offset := 0;
  call_gc_sites := [];
  bound_error_call := 0;
  jit_text();
  jit_align 0 16;
  jit_global fundecl.fun_name;
  jit_symbol fundecl.fun_name;
  let n = frame_size() - 4 in
  if n > 0 then
    jit_subl (Immediate (Nativeint.of_int n)) esp;
  jit_label !tailrec_entry_point;
  emit_all true fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  emit_call_bound_errors()


(* Emission of data *)

let data = Jitaux.data

(* Beginning / end of an assembly file *)

let begin_assembly() =
  float_constants := [];
  Jitaux.begin_assembly()

let end_assembly() =
  jit_data();
  jit_align 0 8;
  List.iter emit_float_constant !float_constants;
  Jitaux.end_assembly()

