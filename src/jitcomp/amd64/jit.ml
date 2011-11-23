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

(* JIT emission of x86-64 (AMD 64) assembly code *)

open Misc
open Cmm
open Arch
open Proc
open Reg
open Mach
open Jitaux
open Emitaux
open Linearize

(* Test if a symbol is local for the current unit *)

let is_local_symbol sym =
  let cun = (Compilenv.make_symbol (Some "")) in
  try cun = (String.sub sym 0 (String.length cun))
  with Invalid_argument _ -> false

(* External address table *)

let externals = ref ([] : (string * label) list)

let jit_external_label_for_symbol sym =
  try
    List.assoc sym !externals
  with
    Not_found ->
      let lbl = new_label() in
      externals := (sym, lbl) :: !externals;
      lbl

(* Instructions *)

type argument =
    Register of int
  | Memory of (*ireg*)int * (*scale*)int * (*breg*)int * (*disp*)int
              (* A value of -1 for breg means no base register *)
  | MemoryTag of tag
  | Immediate of int

let rax = Register 0
let rdx = Register 2
let rsp = Register 4
let r14 = Register 14
let r15 = Register 15
let xmm15 = r15
let al = rax

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

let rex  = 0b01000000
let rexw = 0b01001000
let rexr = 0b00000100
let rexx = 0b00000010
let rexb = 0b00000001

let is_imm8 n = n >= -128 && n <= 127

let jit_rex rex reg_reg reg_index reg_opcode =
  let rex = (rex
              lor (if reg_reg > 7 then rexr else 0)
              lor (if reg_index > 7 then rexx else 0)
              lor (if reg_opcode > 7 then rexb else 0)) in
  if rex != 0 then jit_int8 (rex lor 0b01000000)

let jit_mod_rm_reg rex opcodes rm reg =
  let jit_opcodes opcodes =
    if opcodes land 0x00ff00 != 0 then jit_int8 (opcodes asr 8);
    jit_int8 opcodes
  and jit_modrm m rm reg =
    jit_int8 ((m lsl 6) lor (rm land 7) lor ((reg land 7) lsl 3))
  and jit_sib s i b =
    let s = (match s with
                1 -> 0
              | 2 -> 1
              | 4 -> 2
              | 8 -> 3
              | _ -> assert false) in
    jit_int8 ((s lsl 6) lor ((i land 7) lsl 3) lor (b land 7))
  in match rm with
  | Register rm ->
      jit_rex rex reg 0 rm;
      jit_opcodes opcodes;
      jit_modrm 0b11 rm reg
  | Memory(((*%rsp*)4 | (*%r12*)12) as breg, 1, -1, 0) ->
      jit_rex rex reg 0 breg;
      jit_opcodes opcodes;
      jit_modrm 0b00 breg reg;
      jit_sib 1 0b100 breg
  | Memory(((*%rsp*)4 | (*%r12*)12) as breg, 1, -1, disp) when is_imm8 disp ->
      jit_rex rex reg 0 breg;
      jit_opcodes opcodes;
      jit_modrm 0b01 0b100 reg;
      jit_sib 1 0b100 breg;
      jit_int8 disp
  | Memory(((*%rsp*)4 | (*%r12*)12) as breg, 1, -1, disp) ->
      jit_rex rex reg 0 breg;
      jit_opcodes opcodes;
      jit_modrm 0b10 0b100 reg;
      jit_sib 1 0b100 breg;
      jit_int32 disp
  | Memory(((*%rbp*)5 | (*%r13*)13) as breg, 1, -1, disp) when is_imm8 disp ->
      jit_rex rex reg 0 breg;
      jit_opcodes opcodes;
      jit_modrm 0b01 breg reg;
      jit_int8 disp
  | Memory(rm, 1, -1, 0) ->
      jit_rex rex reg 0 rm;
      jit_opcodes opcodes;
      jit_modrm 0b00 rm reg
  | Memory(rm, 1, -1, disp) when is_imm8 disp ->
      jit_rex rex reg 0 rm;
      jit_opcodes opcodes;
      jit_modrm 0b01 rm reg;
      jit_int8 disp
  | Memory(rm, 1, -1, disp) ->
      jit_rex rex reg 0 rm;
      jit_opcodes opcodes;
      jit_modrm 0b10 rm reg;
      jit_int32 disp
  | Memory(ireg, scale, (*no breg*)-1, disp) ->
      jit_rex rex reg ireg 0;
      jit_opcodes opcodes;
      jit_modrm 0b00 0b100 reg;
      jit_sib scale ireg 0b101;
      jit_int32 disp
  | Memory(ireg, scale, (((*%rbp*)5 | (*%r13*)13) as breg), 0) ->
      jit_rex rex reg ireg breg;
      jit_opcodes opcodes;
      jit_modrm 0b01 0b100 reg;
      jit_sib scale ireg breg;
      jit_int8 0
  | Memory(ireg, scale, breg, 0) ->
      jit_rex rex reg ireg breg;
      jit_opcodes opcodes;
      jit_modrm 0b00 0b100 reg;
      jit_sib scale ireg breg
  | Memory(ireg, scale, breg, disp) when is_imm8 disp ->
      jit_rex rex reg ireg breg;
      jit_opcodes opcodes;
      jit_modrm 0b01 0b100 reg;
      jit_sib scale ireg breg;
      jit_int8 disp
  | Memory(ireg, scale, breg, disp) ->
      jit_rex rex reg ireg breg;
      jit_opcodes opcodes;
      jit_modrm 0b10 0b100 reg;
      jit_sib scale ireg breg;
      jit_int32 disp
  | MemoryTag tag ->
      jit_rex rex reg 0 0;
      jit_opcodes opcodes;
      jit_modrm 0b00 0b101 reg;
      jit_reloc (R_REL_32 tag);
      jit_int32l (-4l)
  | _ ->
      assert false

let jit_testq src dst =
  match src, dst with
    Immediate n, Register (*%rax*)0 when is_imm8 n ->
      jit_int8 0xa8;
      jit_int8 n
  | Immediate n, (Register reg as rm) when is_imm8 n ->
      (* Add REX prefix for %spl, %bpl, %sil and %dil *)
      let rex = if reg >= 4 && reg < 8 then rex else 0 in
      jit_mod_rm_reg rex 0xf6 rm 0;
      jit_int8 n
  | Immediate n, Register (*%rax*)0 ->
      jit_int8 rexw;
      jit_int8 0xa9;
      jit_int32 n
  | Immediate n, rm ->
      jit_mod_rm_reg rexw 0xf7 rm 0;
      jit_int32 n
  | Register reg, rm ->
      jit_mod_rm_reg rexw 0x85 rm reg
  | _ ->
      assert false

let jit_movss src dst =
  jit_int8 0xf3;
  match src, dst with
    rm, Register reg ->
      jit_mod_rm_reg 0 0x0f10 rm reg
  | Register reg, rm ->
      jit_mod_rm_reg 0 0x0f11 rm reg
  | _ ->
      assert false

let jit_movsd src dst =
  jit_int8 0xf2;
  match src, dst with
    rm, Register reg ->
      jit_mod_rm_reg 0 0x0f10 rm reg
  | Register reg, rm ->
      jit_mod_rm_reg 0 0x0f11 rm reg
  | _ ->
      assert false

let jit_rmreg rex opcode src dst =
  match src, dst with
    rm, Register reg ->
      jit_mod_rm_reg rex opcode rm reg
  | _ ->
      assert false

let jit_sse2 prefix rex opcode src dst =
  jit_int8 prefix;
  jit_rmreg rex opcode src dst

let jit_movapd     src dst = jit_sse2 0x66 0 0x0f28 src dst
let jit_xorpd      src dst = jit_sse2 0x66 0 0x0f57 src dst
let jit_andpd      src dst = jit_sse2 0x66 0 0x0f54 src dst
let jit_addsd      src dst = jit_sse2 0xf2 0 0x0f58 src dst
let jit_subsd      src dst = jit_sse2 0xf2 0 0x0f5c src dst
let jit_mulsd      src dst = jit_sse2 0xf2 0 0x0f59 src dst
let jit_divsd      src dst = jit_sse2 0xf2 0 0x0f5e src dst
let jit_cvtsd2ss   src dst = jit_sse2 0xf2 rexw 0x0f5a src dst
let jit_cvtss2sd   src dst = jit_sse2 0xf3 rexw 0x0f2a src dst
let jit_cvtsi2sdq  src dst = jit_sse2 0xf2 rexw 0x0f2a src dst
let jit_cvttsd2siq src dst = jit_sse2 0xf2 rexw 0x0f2c src dst
let jit_comisd     src dst = jit_sse2 0x66 0 0x0f2f src dst
let jit_ucomisd    src dst = jit_sse2 0x66 0 0x0f2e src dst

let jit_movabsq n dst =
  match dst with
    Register reg ->
      jit_rex rexw 0 0 reg;
      jit_int8 (0xb8 lor (reg land 7));
      jit_int64n n
  | _ ->
      assert false

let jit_movb src dst =
  match src, dst with
    Register reg, rm ->
      jit_mod_rm_reg rex 0x88 rm reg
  | _ ->
      assert false

let jit_movw src dst =
  match src, dst with
    Register reg, rm ->
      jit_int8 0x66;
      jit_mod_rm_reg rex 0x89 rm reg
  | _ ->
      assert false

let jit_movl src dst =
  match src, dst with
    rm, Register reg ->
      jit_mod_rm_reg 0 0x8b rm reg
  | Register reg, rm ->
      jit_mod_rm_reg 0 0x89 rm reg
  | _ ->
      assert false

let jit_movq src dst =
  match src, dst with
    Immediate n, ((Memory _ | Register _) as rm) ->
      jit_mod_rm_reg rexw 0xc7 rm 0;
      jit_int32 n
  | rm, Register reg ->
      jit_mod_rm_reg rexw 0x8b rm reg
  | Register reg, rm ->
      jit_mod_rm_reg rexw 0x89 rm reg
  | _ ->
      assert false

let jit_movsbq src dst = jit_rmreg rex  0x0fbe src dst
let jit_movswq src dst = jit_rmreg rexw 0x0fbf src dst
let jit_movslq src dst = jit_rmreg rexw 0x0063 src dst
let jit_movzbq src dst = jit_rmreg rexw 0x0fb6 src dst
let jit_movzwq src dst = jit_rmreg rexw 0x0fb7 src dst
let jit_leaq   src dst = jit_rmreg rexw 0x008d src dst

let jit_aluq op src dst =
  match src, dst with
    Immediate n, rm when is_imm8 n ->
      jit_mod_rm_reg rexw 0x83 rm op;
      jit_int8 n
  | Immediate n, Register (*%rax*)0 ->
      jit_int8 rexw;
      jit_int8 ((op lsl 3) + 5);
      jit_int32 n
  | Immediate n, rm ->
      jit_mod_rm_reg rexw 0x81 rm op;
      jit_int32 n
  | rm, Register reg ->
      jit_mod_rm_reg rexw ((op lsl 3) + 3) rm reg
  | Register reg, rm ->
      jit_mod_rm_reg rexw ((op lsl 3) + 1) rm reg
  | _ ->
      assert false

let jit_addq src dst = jit_aluq 0 src dst
let jit_orq  src dst = jit_aluq 1 src dst
let jit_andq src dst = jit_aluq 4 src dst
let jit_subq src dst = jit_aluq 5 src dst
let jit_xorq src dst = jit_aluq 6 src dst
let jit_cmpq src dst = jit_aluq 7 src dst

let jit_imulq src dst =
  match src, dst with
    Immediate n, (Register reg as rm) when is_imm8 n ->
      jit_mod_rm_reg rexw 0x6b rm reg;
      jit_int8 n
  | Immediate n, (Register reg as rm) ->
      jit_mod_rm_reg rexw 0x69 rm reg;
      jit_int32 n
  | rm, Register reg ->
      jit_mod_rm_reg rexw 0x0faf rm reg
  | _ ->
      assert false

let jit_idivq dst =
  jit_mod_rm_reg rexw 0xf7 dst 7

let jit_shfq op src dst =
  match src, dst with
    Immediate 1, rm ->
      jit_mod_rm_reg rexw 0xd1 rm op
  | Immediate n, rm ->
      jit_mod_rm_reg rexw 0xc1 rm op;
      jit_int8 n
  | (*%cl*)Register 1, rm ->
      jit_mod_rm_reg rexw 0xd3 rm op
  | _ ->
      assert false

let jit_salq src dst = jit_shfq 4 src dst
let jit_shrq src dst = jit_shfq 5 src dst
let jit_sarq src dst = jit_shfq 7 src dst

let jit_jmpq dst =
  jit_mod_rm_reg 0 0xff dst 4

let jit_jmp_tag tag =
  jit_int8 0xe9;
  jit_reloc (R_REL_32 tag);
  jit_int32l (-4l)

let jit_jmp_label lbl =
  jit_jmp_tag (jit_label_tag lbl)

let jit_jmp_symbol sym =
  (* local symbols don't need indirection *)
  if is_local_symbol sym then
    (* jmp sym *)
    jit_jmp_tag (jit_symbol_tag sym)
  else begin
    (* jmpq sym@GOT(%rip) *)
    let lbl = jit_external_label_for_symbol sym in
    jit_jmpq (MemoryTag(jit_label_tag lbl))
  end

let jit_callq dst =
  jit_mod_rm_reg 0 0xff dst 2

let jit_call_tag tag =
  jit_int8 0xe8;
  jit_reloc (R_REL_32 tag);
  jit_int32l (-4l)

let jit_call_label lbl =
  jit_call_tag (jit_label_tag lbl)

let jit_call_symbol sym =
  (* local symbols don't need indirection *)
  if is_local_symbol sym then
    (* call sym *)
    jit_call_tag (jit_symbol_tag sym)
  else begin
    (* callq sym@GOT(%rip) *)
    let lbl = jit_external_label_for_symbol sym in
    jit_callq (MemoryTag(jit_label_tag lbl))
  end

let jit_ret() =
  jit_int8 0xc3

let jit_pushq = function
    Register reg ->
      jit_rex 0 0 0 reg;
      jit_int8 (0x50 + (reg land 0x07))
  | _ ->
      assert false

let jit_popq = function
    Register reg ->
      jit_rex 0 0 0 reg;
      jit_int8 (0x58 + (reg land 0x07))
  | _ ->
      assert false

let jit_cqto() =
  jit_int8 rexw;
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
  jit_reloc (R_REL_32(jit_label_tag lbl));
  jit_int32l (-4l)

let jit_jb_label   lbl = jit_jcc_label B   lbl
let jit_jnb_label  lbl = jit_jcc_label NB  lbl
let jit_jz_label   lbl = jit_jcc_label Z   lbl
let jit_jnz_label  lbl = jit_jcc_label NZ  lbl
let jit_jbe_label  lbl = jit_jcc_label BE  lbl
let jit_jnbe_label lbl = jit_jcc_label NBE lbl
let jit_jp_label   lbl = jit_jcc_label P   lbl
let jit_jnp_label  lbl = jit_jcc_label NP  lbl
let jit_jle_label  lbl = jit_jcc_label LE  lbl
let jit_jnle_label lbl = jit_jcc_label NLE lbl

let jit_cmovcc cc src dst =
  jit_rmreg rexw (0x0f40 + (int_of_cc cc)) src dst

let jit_cmovs  src dst = jit_cmovcc S src dst
let jit_cmovns src dst = jit_cmovcc NS src dst

let jit_setcc cc dst =
  jit_mod_rm_reg 0 (0x0f90 + (int_of_cc cc)) dst 0

(* Generate appropriate code to load the address of sym into dst *)
let jit_load_symbol_addr sym dst =
  (* local symbols don't need indirection *)
  if is_local_symbol sym then
    jit_leaq (MemoryTag(jit_symbol_tag sym)) dst
  else begin
    (* GOT magic... well somewhat *)
    let lbl = jit_external_label_for_symbol sym in
    jit_movq (MemoryTag(jit_label_tag lbl)) dst
  end
 

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

let stack_offset = ref 0

(* Layout of the stack frame *)

let frame_required() =
  !contains_calls || num_stack_slots.(0) > 0 || num_stack_slots.(1) > 0

let frame_size() =                     (* includes return address *)
  if frame_required() then begin
    let sz =
      (!stack_offset + 8 * (num_stack_slots.(0) + num_stack_slots.(1)) + 8)
    in Misc.align sz 16
  end else
    !stack_offset + 8

let slot_offset loc cl =
  match loc with
    Incoming n -> frame_size() + n
  | Local n ->
      if cl = 0
      then !stack_offset + n * 8
      else !stack_offset + (num_stack_slots.(0) + n) * 8
  | Outgoing n -> n


(* Output a pseudo-register *)

(* Map register index to phys index (cf. proc.ml) *)
let int_reg_index =
  [| (*%rax*) 0; (*%rbx*) 3; (*%rdi*) 7; (*%rsi*) 6;
     (*%rdx*) 2; (*%rcx*) 1; (*%r8 *) 8; (*%r9 *) 9;
     (*%r10*)10; (*%r11*)11; (*%rbp*) 5; (*%r12*)12;
     (*%r13*)13 |]

let register_index r =
  if r < 100 then int_reg_index.(r) else r - 100

let emit_reg = function
    { loc = Reg r } ->
      Register(register_index r)
  | { loc = Stack s } as r ->
      let ofs = slot_offset s (register_class r) in
      membase ofs rsp
  | { loc = Unknown } ->
      assert false

(* Output an addressing mode *)

let emit_addressing addr r n =
  match addr with
  | Ibased _ ->
      assert false
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

(* Record live pointers at call points -- see Emitaux *)

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
    jit_call_symbol "caml_ml_array_bound_error";
  end

(* Names for instructions *)

let instr_for_intop = function
    Iadd -> jit_addq
  | Isub -> jit_subq
  | Imul -> jit_imulq
  | Iand -> jit_andq
  | Ior  -> jit_orq
  | Ixor -> jit_xorq
  | Ilsl -> jit_salq
  | Ilsr -> jit_shrq
  | Iasr -> jit_sarq
  | _ -> assert false

let instr_for_floatop = function
    Iaddf -> jit_addsd
  | Isubf -> jit_subsd
  | Imulf -> jit_mulsd
  | Idivf -> jit_divsd
  | _ -> assert false

let instr_for_floatarithmem = function
    Ifloatadd -> jit_addsd
  | Ifloatsub -> jit_subsd
  | Ifloatmul -> jit_mulsd
  | Ifloatdiv -> jit_divsd

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
    Reg _ -> jit_testq (emit_reg arg) (emit_reg arg)
  | _     -> jit_cmpq (Immediate 0) (emit_reg arg)

(* Output a floating-point compare and branch *)

let emit_float_test cmp neg arg lbl =
  (* Effect of comisd on flags and conditional branches:
                     ZF PF CF  cond. branches taken
        unordered     1  1  1  je, jb, jbe, jp
        >             0  0  0  jne, jae, ja
        <             0  0  1  jne, jbe, jb
        =             1  0  0  je, jae, jbe.
     If FP traps are on (they are off by default),
     comisd traps on QNaN and SNaN but ucomisd traps on SNaN only.
  *)
  match (cmp, neg) with
  | (Ceq, false) | (Cne, true) ->
      let next = new_label() in
      jit_ucomisd (emit_reg arg.(1)) (emit_reg arg.(0));
      jit_jp_label next;    (* skip if unordered *)
      jit_jz_label lbl;     (* branch taken if x=y *)
      jit_label next
  | (Cne, false) | (Ceq, true) ->
      jit_ucomisd (emit_reg arg.(1)) (emit_reg arg.(0));
      jit_jp_label lbl;     (* branch taken if unordered *)
      jit_jnz_label lbl     (* branch taken if x<y or x>y *)
  | (Clt, _) ->
      jit_comisd (emit_reg arg.(0)) (emit_reg arg.(1));  (* swap compare *)
      if not neg then
        jit_jnbe_label lbl  (* branch taken if y>x i.e. x<y *)
      else
        jit_jbe_label lbl   (* taken if unordered or y<=x i.e. !(x<y) *)
  | (Cle, _) ->
      jit_comisd (emit_reg arg.(0)) (emit_reg arg.(1));  (* swap compare *)
      if not neg then
        jit_jnb_label lbl   (* branch taken if y>=x i.e. x<=y *)
      else
        jit_jb_label lbl    (* taken if unordered or y<x i.e. !(x<=y) *)
  | (Cgt, _) ->
      jit_comisd (emit_reg arg.(1)) (emit_reg arg.(0));
      if not neg then
        jit_jnbe_label lbl  (* branch taken if x>y *)
      else
        jit_jbe_label lbl   (* taken if unordered or x<=y i.e. !(x>y) *)
  | (Cge, _) ->
      jit_comisd (emit_reg arg.(1)) (emit_reg arg.(0));  (* swap compare *)
      if not neg then
        jit_jnb_label lbl   (* branch taken if x>=y *)
      else
        jit_jb_label lbl    (* taken if unordered or x<y i.e. !(x>=y) *)

(* Deallocate the stack frame before a return or tail call *)

let output_epilogue() =
  if frame_required() then begin
    let n = frame_size() - 8 in
    jit_addq (Immediate n) rsp
  end

(* Output the assembly code for an instruction *)

(* Name of current function *)
let function_name = ref ""
(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref 0

let float_constants = ref ([] : (label * string) list)

(* Labels for caml_absf_mask and caml_negf_mask *)
let absf_mask_lbl = ref (0 : label)
let negf_mask_lbl = ref (0 : label)

let emit_instr fallthrough i =
    match i.desc with
      Lend ->()
    | Lop(Imove | Ispill | Ireload) ->
        let src = i.arg.(0) and dst = i.res.(0) in
        if src.loc <> dst.loc then begin
          match src.typ, src.loc, dst.loc with
            Float, Reg _, Reg _ ->
              jit_movapd (emit_reg src) (emit_reg dst)
          | Float, _, _ ->
              jit_movsd (emit_reg src) (emit_reg dst)
          | _ ->
              jit_movq (emit_reg src) (emit_reg dst)
        end
    | Lop(Iconst_int n) ->
        begin match n, i.res.(0).loc with
        | 0n, Reg _ ->
            jit_xorq (emit_reg i.res.(0)) (emit_reg i.res.(0))
        | n, _ when n <= 0x7FFFFFFFn && n >= -0x80000000n ->
            jit_movq (Immediate (Nativeint.to_int n)) (emit_reg i.res.(0))
        | n, _ ->
            jit_movabsq n (emit_reg i.res.(0))
        end
    | Lop(Iconst_float s) ->
        begin match Int64.bits_of_float (float_of_string s) with
        | 0x0000_0000_0000_0000L ->       (* +0.0 *)
          jit_xorpd (emit_reg i.res.(0)) (emit_reg i.res.(0))
        | _ ->
          let lbl = new_label() in
          float_constants := (lbl, s) :: !float_constants;
          jit_movsd (MemoryTag(jit_label_tag lbl)) (emit_reg i.res.(0))
        end
    | Lop(Iconst_symbol s) ->
        jit_load_symbol_addr s (emit_reg i.res.(0))
    | Lop(Icall_ind) ->
        jit_callq (emit_reg i.arg.(0));
        record_frame i.live i.dbg
    | Lop(Icall_imm(s)) ->
        jit_call_symbol s;
        record_frame i.live i.dbg
    | Lop(Itailcall_ind) ->
        output_epilogue();
        jit_jmpq (emit_reg i.arg.(0))
    | Lop(Itailcall_imm s) ->
        if s = !function_name then
          jit_jmp_label !tailrec_entry_point
        else begin
          output_epilogue();
          jit_jmp_symbol s
        end
    | Lop(Iextcall(s, alloc)) ->
        if alloc then begin
          jit_load_symbol_addr s rax;
          jit_call_symbol "caml_c_call";
          record_frame i.live i.dbg
        end else begin
          jit_call_symbol s
        end
    | Lop(Istackoffset n) ->
        jit_subq (Immediate n) rsp;
        stack_offset := !stack_offset + n
    | Lop(Iload(chunk, addr)) ->
        let dest = i.res.(0) in
        begin match chunk with
          | Word ->
              jit_movq (emit_addressing addr i.arg 0) (emit_reg dest)
          | Byte_unsigned ->
              jit_movzbq (emit_addressing addr i.arg 0) (emit_reg dest)
          | Byte_signed ->
              jit_movsbq (emit_addressing addr i.arg 0) (emit_reg dest)
          | Sixteen_unsigned ->
              jit_movzwq (emit_addressing addr i.arg 0) (emit_reg dest)
          | Sixteen_signed ->
              jit_movswq (emit_addressing addr i.arg 0) (emit_reg dest)
          | Thirtytwo_unsigned ->
              jit_movl (emit_addressing addr i.arg 0) (emit_reg dest)
          | Thirtytwo_signed ->
              jit_movslq (emit_addressing addr i.arg 0) (emit_reg dest)
          | Single ->
              jit_cvtss2sd (emit_addressing addr i.arg 0) (emit_reg dest)
          | Double | Double_u ->
              jit_movsd (emit_addressing addr i.arg 0) (emit_reg dest)
        end
    | Lop(Istore(chunk, addr)) ->
        begin match chunk with
          | Word ->
            jit_movq (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
          | Byte_unsigned | Byte_signed ->
            jit_movb (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
          | Sixteen_unsigned | Sixteen_signed ->
            jit_movw (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
          | Thirtytwo_signed | Thirtytwo_unsigned ->
            jit_movl (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
          | Single ->
            jit_cvtsd2ss (emit_reg i.arg.(0)) xmm15;
            jit_movss xmm15 (emit_addressing addr i.arg 1)
          | Double | Double_u ->
            jit_movsd (emit_reg i.arg.(0)) (emit_addressing addr i.arg 1)
        end
    | Lop(Ialloc n) ->
        if !fastcode_flag then begin
          let lbl_redo = new_label() in begin
            jit_label lbl_redo;
            jit_subq (Immediate n) r15
          end;
          jit_load_symbol_addr "caml_young_limit" rax;
          jit_cmpq (membase 0 rax) r15;
          let lbl_call_gc = new_label() in
          let lbl_frame = record_frame_label i.live Debuginfo.none in
          jit_jb_label lbl_call_gc;
          jit_leaq (membase 8 r15) (emit_reg i.res.(0));
          call_gc_sites :=
            { gc_lbl = lbl_call_gc;
              gc_return_lbl = lbl_redo;
              gc_frame = lbl_frame } :: !call_gc_sites
        end else begin
          begin match n with
            16 -> jit_call_symbol "caml_alloc1";
          | 24 -> jit_call_symbol "caml_alloc2";
          | 32 -> jit_call_symbol "caml_alloc3";
          | n  -> jit_movq (Immediate n) rax;
                  jit_call_symbol "caml_allocN";
          end;
          record_frame i.live Debuginfo.none;
          jit_leaq (membase 8 r15) (emit_reg i.res.(0))
        end
    | Lop(Iintop(Icomp cmp)) ->
        jit_cmpq (emit_reg i.arg.(1)) (emit_reg i.arg.(0));
        let cc = cc_for_cond_branch cmp in
        jit_setcc cc al;
        jit_movzbq al (emit_reg i.res.(0))
    | Lop(Iintop_imm(Icomp cmp, n)) ->
        jit_cmpq (Immediate n) (emit_reg i.arg.(0));
        let cc = cc_for_cond_branch cmp in
        jit_setcc cc al;
        jit_movzbq al (emit_reg i.res.(0))
    | Lop(Iintop Icheckbound) ->
        let lbl = bound_error_label i.dbg in
        jit_cmpq (emit_reg i.arg.(1)) (emit_reg i.arg.(0));
        jit_jbe_label lbl
    | Lop(Iintop_imm(Icheckbound, n)) ->
        let lbl = bound_error_label i.dbg in
        jit_cmpq (Immediate n) (emit_reg i.arg.(0));
        jit_jbe_label lbl
    | Lop(Iintop(Idiv | Imod)) ->
        jit_cqto();
        jit_idivq (emit_reg i.arg.(1))
    | Lop(Iintop(Ilsl | Ilsr | Iasr as op)) ->
        (* We have i.arg.(0) = i.res.(0) and i.arg.(1) = %rcx *)
        (instr_for_intop op) (emit_reg i.arg.(1)) (emit_reg i.res.(0))
    | Lop(Iintop op) ->
        (* We have i.arg.(0) = i.res.(0) *)
        (instr_for_intop op) (emit_reg i.arg.(1)) (emit_reg i.res.(0))
    | Lop(Iintop_imm(Iadd, n)) when i.arg.(0).loc <> i.res.(0).loc ->
        jit_leaq (membase n (emit_reg i.arg.(0))) (emit_reg i.res.(0))
    | Lop(Iintop_imm(Idiv, n)) ->
        (* Note: i.arg.(0) = i.res.(0) = rdx  (cf. selection.ml) *)
        let l = Misc.log2 n in
        jit_movq (emit_reg i.arg.(0)) rax;
        jit_addq (Immediate (n - 1)) (emit_reg i.arg.(0));
        jit_testq rax rax;
        jit_cmovns rax (emit_reg i.arg.(0));
        jit_sarq (Immediate l) (emit_reg i.res.(0))
    | Lop(Iintop_imm(Imod, n)) ->
        (* Note: i.arg.(0) = i.res.(0) = rdx  (cf. selection.ml) *)
        jit_movq (emit_reg i.arg.(0)) rax;
        jit_testq rax rax;
        jit_leaq (membase (n - 1) rax) rax;
        jit_cmovns (emit_reg i.arg.(0)) rax;
        jit_andq (Immediate (-n)) rax;
        jit_subq rax (emit_reg i.res.(0))
    | Lop(Iintop_imm(op, n)) ->
        (* We have i.arg.(0) = i.res.(0) *)
        (instr_for_intop op) (Immediate n) (emit_reg i.res.(0))
    | Lop(Iabsf) ->
        if !absf_mask_lbl == 0 then absf_mask_lbl := new_label();
        jit_andpd (MemoryTag(jit_label_tag !absf_mask_lbl)) (emit_reg i.res.(0))
    | Lop(Inegf) ->
        if !negf_mask_lbl == 0 then negf_mask_lbl := new_label();
        jit_xorpd (MemoryTag(jit_label_tag !negf_mask_lbl)) (emit_reg i.res.(0))
    | Lop(Iaddf | Isubf | Imulf | Idivf as floatop) ->
        (instr_for_floatop floatop) (emit_reg i.arg.(1)) (emit_reg i.res.(0))
    | Lop(Ifloatofint) ->
        jit_cvtsi2sdq (emit_reg i.arg.(0)) (emit_reg i.res.(0))
    | Lop(Iintoffloat) ->
        jit_cvttsd2siq (emit_reg i.arg.(0)) (emit_reg i.res.(0))
    | Lop(Ispecific(Ilea addr)) ->
        jit_leaq (emit_addressing addr i.arg 0) (emit_reg i.res.(0))
    | Lop(Ispecific(Istore_int(n, addr))) ->
        jit_movq (Immediate (Nativeint.to_int n)) (emit_addressing addr i.arg 0)
    | Lop(Ispecific(Istore_symbol(s, addr))) ->
        assert false
    | Lop(Ispecific(Ioffset_loc(n, addr))) ->
        jit_addq (Immediate n) (emit_addressing addr i.arg 0)
    | Lop(Ispecific(Ifloatarithmem(op, addr))) ->
        (instr_for_floatarithmem op) (emit_addressing addr i.arg 1) (emit_reg i.res.(0))
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
            jit_cmpq (emit_reg i.arg.(1)) (emit_reg i.arg.(0));
            let cc = cc_for_cond_branch cmp in
            jit_jcc_label cc lbl
        | Iinttest_imm((Isigned Ceq | Isigned Cne |
                        Iunsigned Ceq | Iunsigned Cne) as cmp, 0) ->
            output_test_zero i.arg.(0);
            let cc = cc_for_cond_branch cmp in
            jit_jcc_label cc lbl
        | Iinttest_imm(cmp, n) ->
            jit_cmpq (Immediate n) (emit_reg i.arg.(0));
            let cc = cc_for_cond_branch cmp in
            jit_jcc_label cc lbl
        | Ifloattest(cmp, neg) ->
            emit_float_test cmp neg i.arg lbl
        | Ioddtest ->
            jit_testq (Immediate 1) (emit_reg i.arg.(0));
            jit_jnz_label lbl
        | Ieventest ->
            jit_testq (Immediate 1) (emit_reg i.arg.(0));
            jit_jz_label lbl
        end
    | Lcondbranch3(lbl0, lbl1, lbl2) ->
        jit_cmpq (Immediate 1) (emit_reg i.arg.(0));
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
        (* rax and rdx are clobbered by the Lswitch,
           meaning that no variable that is live across the Lswitch
           is assigned to rax or rdx.  However, the argument to Lswitch
           can still be assigned to one of these two registers, so
           we must be careful not to clobber it before use. *)
        let (tmp1, tmp2) =
          if i.arg.(0).loc = Reg 0 (* rax *)
          then (rdx, rax)
          else (rax, rdx) in
        jit_leaq (MemoryTag(jit_label_tag lbl)) tmp1;
        jit_movslq (memindex 0 tmp1 (emit_reg i.arg.(0)) 4) tmp2;
        jit_addq tmp2 tmp1;
        jit_jmpq tmp1;
        jit_data();
        jit_align 0 4;
        jit_label lbl;
        for i = 0 to Array.length jumptbl - 1 do
          (* .long jumptbl.(i) - lbl *)
          jit_reloc (R_REL_32(jit_label_tag jumptbl.(i)));
          jit_int32 (4 * i)
        done;
        jit_text()
    | Lsetuptrap lbl ->
        jit_call_label lbl
    | Lpushtrap ->
        jit_pushq r14;
        jit_movq rsp r14;
        stack_offset := !stack_offset + 16
    | Lpoptrap ->
        jit_popq r14;
        jit_addq (Immediate 8) rsp;
        stack_offset := !stack_offset - 16
    | Lraise ->
        jit_movq r14 rsp;
        jit_popq r14;
        jit_ret()

let rec emit_all fallthrough i =
  match i.desc with
  |  Lend -> ()
  | _ ->
      emit_instr fallthrough i;
      emit_all (Linearize.has_fallthrough i.desc) i.next

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
  if frame_required() then begin
    let n = frame_size() - 8 in
    jit_subq (Immediate n) rsp
  end;
  jit_label !tailrec_entry_point;
  emit_all true fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  emit_call_bound_errors()

(* Emission of data *)

let data = Jitaux.data

(* Beginning / end of an assembly file *)

let begin_assembly() =
  externals := [];
  float_constants := [];
  absf_mask_lbl := 0;
  negf_mask_lbl := 0;
  Jitaux.begin_assembly()

let end_assembly() =
  jit_data();
  jit_align 0 16;
  (* from amd64.S; emit these constants on demand *)
  if !absf_mask_lbl != 0 then begin
    jit_label !absf_mask_lbl;
    jit_int64L 0x7FFFFFFFFFFFFFFFL; jit_int64L 0xFFFFFFFFFFFFFFFFL
  end;
  if !negf_mask_lbl != 0 then begin
    jit_label !negf_mask_lbl;
    jit_int64L 0x8000000000000000L; jit_int64L 0x0000000000000000L
  end;
  (* Output the external address table *)
  List.iter (fun (sym, lbl) ->
               jit_label lbl;
               jit_reloc (R_ABS_64(jit_symbol_tag sym));
               jit_int64L 0L) !externals;
  (* Emit floating point constants *)
  List.iter emit_float_constant !float_constants;
  Jitaux.end_assembly()

