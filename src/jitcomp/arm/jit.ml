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

(* JIT emission of ARM assembly code *)

open Location
open Misc
open Cmm
open Arch
open Proc
open Reg
open Mach
open Jitaux
open Linearize
open Emitaux

module Nativeint =
struct
  include Nativeint

  let rotate_left x y =
    Nativeint.logor
      (Nativeint.shift_left x y)
      (Nativeint.shift_right_logical x (32 - y))

  let rotate_right x y =
    Nativeint.logor
      (Nativeint.shift_right_logical x y)
      (Nativeint.shift_left x (32 - y))
end

(* Trampolines *)

let trampolines = ref ([] : (string * label) list)

let jit_trampoline sym =
  try
    List.assoc sym !trampolines
  with
    Not_found ->
      let lbl = new_label() in
      trampolines := (sym, lbl) :: !trampolines;
      lbl

(* Relocations *)

let r_arm_jmp_24 tag =
  let fn s p i =
    let a = (Int32.to_int i) lsl 2 in
    let a = if (a land 0x2000000) == 0 (* sign extend *)
            then a land 0x3fffffc
            else a lor 0x7c000000 in
    let x = Addr.sub (Addr.add_int s a) p in
    assert (x >= -33554432n && x <= 33554431n);
    Int32.logor
      (Int32.logand i 0xff000000l)
      (Addr.to_int32 (Addr.logand
                       (Addr.shift_right x 2)
                       0xffffffn)) in
  R_FUN_32(tag, fn)

let r_arm_ldr_12 tag =
  let fn s p i =
    let a = (Int32.to_int i) land 0xfff in
    let a = if (Int32.logand i 0x800000l) = 0l (* up/down bit *)
            then (-a)
            else a in
    let x = Addr.sub (Addr.add_int s a) p in
    assert (x > -4096n && x < 4096n);
    Int32.logor
      (Int32.logand i 0xff7ff000l)
      (Int32.logand
        (if x < 0n
         then Addr.to_int32 (Addr.neg x)
         else (Int32.logor (Addr.to_int32 x) 0x800000l))
        0x800fffl) in
  R_FUN_32(tag, fn)

(* Instructions *)

(* Condition codes *)
type cc =
    EQ | NE | CS | CC
  | MI | PL | VS | VC
  | HI | LS | GE | LT
  | GT | LE | AL | NV

external int_of_cc: cc -> int = "%identity"

type shift =
    LSL | LSR | ASR | ROR

external int_of_shift: shift -> int = "%identity"

type operand =
    Register of int
  | Immediate of nativeint
  | Shift of operand * shift * operand
  | Memory of (*rn*)operand * (*offset*)operand
  | MemoryTag of (*tag*)tag * (*addend*)int

let regindex = function
    Register r -> assert (r >= 0 && r < 16); r
  | _ -> assert false

let r0  = Register 0
and r1  = Register 1
and r8  = Register 8
and r10 = Register 10
and r11 = Register 11
and r12 = Register 12
and r13 = Register 13
and r14 = Register 14
and r15 = Register 15

let alloc_ptr = r8
and alloc_limit = r10
and trap_ptr = r11
and sp = r13
and lr = r14
and pc = r15

let jit_instr ?cc:(cc=AL) opcode =
  assert (opcode >= 0 && opcode < 0x10000000);
  let cond = Int32.shift_left (Int32.of_int (int_of_cc cc)) 28 in
  jit_int32l (Int32.logor cond (Int32.of_int opcode))

(* Branch instructions *)

let jit_b_tag ?cc:(cc=AL) ?link:(link=false) tag =
  jit_reloc (r_arm_jmp_24 tag);
  jit_instr ~cc (if link then 0x0bfffffe else 0x0afffffe)

let jit_b_label ?cc:(cc=AL) ?link:(link=false) lbl =
  jit_b_tag ~cc ~link (jit_label_tag lbl)

let jit_beq_label lbl = jit_b_label ~cc:EQ lbl
let jit_bne_label lbl = jit_b_label ~cc:NE lbl
let jit_bpl_label lbl = jit_b_label ~cc:PL lbl
let jit_blt_label lbl = jit_b_label ~cc:LT lbl
let jit_bgt_label lbl = jit_b_label ~cc:GT lbl

let jit_bl_label lbl = jit_b_label ~link:true lbl

let jit_b_symbol ?cc:(cc=AL) ?link:(link=false) sym =
  let lbl = jit_trampoline sym in
  jit_b_tag ~cc ~link (jit_label_tag lbl)

let jit_bl_symbol   sym = jit_b_symbol ~link:true sym
let jit_blcc_symbol sym = jit_b_symbol ~cc:CC ~link:true sym
let jit_blcs_symbol sym = jit_b_symbol ~cc:CS ~link:true sym
let jit_blls_symbol sym = jit_b_symbol ~cc:LS ~link:true sym

let jit_bx ?cc:(cc=AL) rn =
  jit_instr ~cc (0x12fff10 lor (regindex rn))

(* Data processing instructions *)

let jit_alu ?cc:(cc=AL) rn rd operand2 opcode =
  let opcode = ((regindex rn) lsl 16)
           lor ((regindex rd) lsl 12)
           lor (match operand2 with
                  Register rm ->
                    assert (rm >= 0 && rm < 16);
                    rm
                | Immediate n ->
                    let rec rotate r n =
                      assert (r >= 0 && r < 32);
                      assert (r mod 2 == 0);
                      if Nativeint.logand n 0xffn = n then begin
                        0x2000000
                        lor ((r lsr 1) lsl 8)
                        lor (Nativeint.to_int n)
                      end else begin
                        let n = Nativeint.rotate_left n 2 in
                        rotate (r + 2) n
                      end in
                    rotate 0 n
                | Shift(rm, shift, Immediate n) ->
                    assert (n >= 0n && n < 32n);
                    ((Nativeint.to_int n) lsl 7)
                    lor ((int_of_shift shift) lsl 5)
                    lor (regindex rm)
                | Shift(rm, shift, rs) ->
                    ((regindex rs) lsl 8)
                    lor ((int_of_shift shift) lsl 5)
                    lor 0b10000
                    lor (regindex rm)
                | _ ->
                    assert false)
           lor (opcode lsl 20) in
  jit_instr ~cc opcode

let jit_and   ?cc:(cc=AL) rd rn operand2 = jit_alu ~cc    rn rd operand2 0b00000
let jit_eor   ?cc:(cc=AL) rd rn operand2 = jit_alu ~cc    rn rd operand2 0b00010
let jit_sub   ?cc:(cc=AL) rd rn operand2 = jit_alu ~cc    rn rd operand2 0b00100
let jit_subne             rd rn operand2 = jit_sub ~cc:NE rn rd operand2
let jit_sublt             rd rn operand2 = jit_sub ~cc:LT rn rd operand2
let jit_rsb   ?cc:(cc=AL) rd rn operand2 = jit_alu ~cc    rn rd operand2 0b00110
let jit_add   ?cc:(cc=AL) rd rn operand2 = jit_alu ~cc    rn rd operand2 0b01000
let jit_addlt             rd rn operand2 = jit_add ~cc:LT rn rd operand2
let jit_tst   ?cc:(cc=AL)    rn operand2 = jit_alu ~cc    rn r0 operand2 0b10001
let jit_cmp   ?cc:(cc=AL)    rn operand2 = jit_alu ~cc    rn r0 operand2 0b10101
let jit_orr   ?cc:(cc=AL) rd rn operand2 = jit_alu ~cc    rn rd operand2 0b11000
let jit_mov   ?cc:(cc=AL) rd    operand2 = jit_alu ~cc    r0 rd operand2 0b11010
let jit_movs  ?cc:(cc=AL) rd    operand2 = jit_alu ~cc    r0 rd operand2 0b11011
let jit_bic   ?cc:(cc=AL) rd rn operand2 = jit_alu ~cc    rn rd operand2 0b11100
let jit_mvn   ?cc:(cc=AL) rd    operand2 = jit_alu ~cc    r0 rd operand2 0b11110

(* Multiply instructions *)

let jit_mul ?cc:(cc=AL) rd rm rs =
  let opcode = ((regindex rd) lsl 16)
           lor ((regindex rs) lsl 8)
           lor 0b10010000
           lor (regindex rm) in
  jit_instr ~cc opcode

(* Single Data Transfer instructions *)

let jit_xfer ?cc:(cc=AL) rd address opcode =
  let opcode = opcode
           lor ((regindex rd) lsl 12)
           lor (match address with
                  Register rn ->
                    0x1000000
                    lor (rn lsl 16)
                | Memory(rn, Immediate n) ->
                    let x = if n < 0n then Nativeint.neg n else n in
                    assert (x >= 0n && x < 4096n);
                    0x1000000
                    lor ((regindex rn) lsl 16)
                    lor (Nativeint.to_int x)
                    lor (if n < 0n then 0 else 0x800000)
                | Memory(rn, Shift(rm, shift, Immediate n)) ->
                    assert (n >= 0n && n < 32n);
                    0x3800000
                    lor ((regindex rn) lsl 16)
                    lor ((Nativeint.to_int n) lsl 7)
                    lor ((int_of_shift shift) lsl 5)
                    lor (regindex rm)
                | Memory(rn, Shift(rm, shift, rs)) ->
                    0x3800010
                    lor ((regindex rn) lsl 16)
                    lor ((regindex rs) lsl 8)
                    lor ((int_of_shift shift) lsl 5)
                    lor (regindex rm)
                | MemoryTag(tag, addend) ->
                    (* r15 contains an address 8 bytes on from
                       the address of the current instruction *)
                    jit_reloc (r_arm_ldr_12 tag);
                    let addend = addend - 8 in
                    assert (addend >= (-4096) && addend <= 4096);
                    0x10f0000
                    lor (if addend < 0 then (-addend) else addend)
                    lor (if addend < 0 then 0 else 0x800000)
                | _ ->
                    assert false) in
  jit_instr ~cc opcode

let jit_ldr  ?cc:(cc=AL) rd address = jit_xfer ~cc rd address 0x4100000
let jit_ldrb ?cc:(cc=AL) rd address = jit_xfer ~cc rd address 0x4500000
let jit_str  ?cc:(cc=AL) rd address = jit_xfer ~cc rd address 0x4000000
let jit_strb ?cc:(cc=AL) rd address = jit_xfer ~cc rd address 0x4400000

(* Halfword and Signed Data Transfer *)

let jit_hsxfer ?cc:(cc=AL) rd address opcode =
  let opcode = opcode
           lor ((regindex rd) lsl 12)
           lor (match address with
                  Memory(rn, Immediate n) ->
                    let x = if n < 0n then Nativeint.neg n else n in
                    assert (x >= 0n);
                    assert (x < 256n);
                    ((regindex rn) lsl 16)
                    lor (Nativeint.to_int
                         (Nativeint.logand x 0x0fn))
                    lor (Nativeint.to_int
                         (Nativeint.shift_left
                          (Nativeint.logand x 0xf0n)
                          4))
                    lor (if n < 0n then 0 else 0x800000)
                | _ ->
                    assert false) in
  jit_instr ~cc opcode

let jit_ldrh  ?cc:(cc=AL) rd address = jit_hsxfer ~cc rd address 0x5000b0
let jit_ldrsb ?cc:(cc=AL) rd address = jit_hsxfer ~cc rd address 0x5000d0
let jit_ldrsh ?cc:(cc=AL) rd address = jit_hsxfer ~cc rd address 0x5000f0
let jit_strh  ?cc:(cc=AL) rd address = jit_hsxfer ~cc rd address 0x4000b0

(* Block Data Transfer instructions *)

let rec regmask = function
    [] -> 0
  | r :: rl -> (1 lsl (regindex r)) lor (regmask rl)

let jit_ldmfd ?cc:(cc=AL) ?wb:(wb=false) rn rl =
  let opcode = if wb then 0x8b00000 else 0x8900000 in
  jit_instr ~cc (opcode lor ((regindex rn) lsl 16) lor (regmask rl))

let jit_stmfd ?cc:(cc=AL) ?wb:(wb=false) rn rl =
  let opcode = if wb then 0x9200000 else 0x9000000 in
  jit_instr ~cc (opcode lor ((regindex rn) lsl 16) lor (regmask rl))


(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Output a pseudo-register *)

(* Map register index to phys index (cf. proc.ml) *)
let register_index r =
  if r < 8 then r else 12

let emit_reg = function
    { loc = Reg r } ->
      Register(register_index r)
  | _ ->
      assert false

(* Layout of the stack frame *)

let stack_offset = ref 0

let frame_size () =
  let sz =
    !stack_offset +
    4 * num_stack_slots.(0) +
    (if !contains_calls then 4 else 0)
  in Misc.align sz 8

let slot_offset loc cl =
  match loc with
    Incoming n -> frame_size() + n
  | Local n -> !stack_offset + n * 4
  | Outgoing n -> n

(* Output a stack reference *)

let emit_stack r =
  match r.loc with
  | Stack s ->
      let ofs = slot_offset s (register_class r) in Memory(sp, Immediate(Nativeint.of_int ofs))
  | _ -> fatal_error "Emit_arm.emit_stack"

(* Output an addressing mode *)

let emit_addressing addr r n =
  match addr with
    Iindexed ofs ->
      Memory(emit_reg r.(n), Immediate(Nativeint.of_int ofs))

(* Record live pointers at call points *)

let record_frame_label live dbg =
  let lbl = new_label() in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
        {typ = Addr; loc = Reg r} ->
          live_offset := (r lsl 1) + 1 :: !live_offset
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

(* Names of various instructions *)

let cc_for_comparison = function
    Isigned   Ceq -> EQ | Isigned   Cne -> NE
  | Isigned   Cle -> LE | Isigned   Cge -> GE
  | Isigned   Clt -> LT | Isigned   Cgt -> GT
  | Iunsigned Ceq -> EQ | Iunsigned Cne -> NE
  | Iunsigned Cle -> LS | Iunsigned Cge -> CS
  | Iunsigned Clt -> CC | Iunsigned Cgt -> HI

let instr_for_intop = function
    Iadd -> jit_add
  | Isub -> jit_sub
  | Imul -> jit_mul
  | Iand -> jit_and
  | Ior  -> jit_orr
  | Ixor -> jit_eor
  | _ -> assert false

let shift_for_shift_operation = function
    Ilsl -> LSL
  | Ilsr -> LSR
  | Iasr -> ASR
  | _ -> assert false

let instr_for_shift_intop = function
    Ishiftadd    -> jit_add
  | Ishiftsub    -> jit_sub
  | Ishiftsubrev -> jit_rsb

(* Recognize immediate operands *)

(* Immediate operands are 8-bit immediate values, zero-extended, and rotated
   right by 0, 2, 4, ... 30 bits. *)

let rec is_immed n shift =
  shift <= 30
  && (Nativeint.logand n 0xffn = n
   || is_immed (Nativeint.rotate_right n 2) (shift + 2))

let is_immediate n = is_immed n 0

(* General functional to decompose a non-immediate integer constant
   into 8-bit chunks shifted left 0 ... 24 bits *)

let decompose_intconst n fn =
  let i = ref n in
  let shift = ref 0 in
  let ninstr = ref 0 in
  while !i <> 0n do
    if Nativeint.to_int (Nativeint.shift_right !i !shift) land 3 = 0 then
      shift := !shift + 2
    else begin
      let mask = Nativeint.shift_left 0xFFn !shift in
      let bits = Nativeint.logand !i mask in
      fn bits;
      shift := !shift + 8;
      i := Nativeint.sub !i bits;
      incr ninstr
    end
  done;
  !ninstr

(* Load an integer constant into a register *)

let emit_intconst r n =
  let nr = Nativeint.lognot n in
  if is_immediate n then begin
    jit_mov (emit_reg r) (Immediate n); 1
  end else if is_immediate nr then begin
    jit_mvn (emit_reg r) (Immediate nr); 1
  end else begin
    let first = ref true in
    decompose_intconst n
      (fun bits ->
        let bits = Immediate bits in
        if !first
        then jit_mov (emit_reg r) bits
        else jit_add (emit_reg r) (emit_reg r) bits;
        first := false)
  end

(* Adjust sp (up or down) by the given byte amount *)

let emit_stack_adjustment instr n =
  if n <= 0 then 0 else
    decompose_intconst (Nativeint.of_int n)
      (fun bits -> instr sp sp (Immediate bits))

(* Name of current function *)
let function_name = ref ""
(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref 0
(* Table of symbols referenced *)
let symbol_constants = (Hashtbl.create 11 : (string, int) Hashtbl.t)
(* Table of floating-point literals *)
let float_constants = (Hashtbl.create 11 : (string, int) Hashtbl.t)
(* Total space (in word) occupied by pending literals *)
let num_literals = ref 0

(* Label a symbol or float constant *)
let label_constant tbl s size =
  try
    Hashtbl.find tbl s
  with Not_found ->
    let lbl = new_label() in
    Hashtbl.add tbl s lbl;
    num_literals := !num_literals + size;
    lbl

(* Emit all pending constants *)

let emit_constants () =
  Hashtbl.iter
    (fun s lbl ->
      jit_label lbl;
      jit_reloc (R_ABS_32(jit_symbol_tag s));
      jit_int32l 0l)
    symbol_constants;
  Hashtbl.iter
    (fun s lbl ->
      jit_label lbl;
      jit_int64L (Int64.bits_of_float (float_of_string s)))
    float_constants;
  Hashtbl.clear symbol_constants;
  Hashtbl.clear float_constants;
  num_literals := 0

(* Output the assembly code for an instruction *)

let emit_instr i =
    match i.desc with
      Lend -> 0
    | Lop(Imove | Ispill | Ireload) ->
        let src = i.arg.(0) and dst = i.res.(0) in
        if src.loc = dst.loc then 0 else begin
          match (src, dst) with
            {loc = Reg rs; typ = Int|Addr}, {loc = Reg rd; typ = Int|Addr} ->
              jit_mov (emit_reg dst) (emit_reg src); 1
          | {loc = Reg rs; typ = Int|Addr}, {loc = Stack sd} ->
              jit_str (emit_reg src) (emit_stack dst); 1
          | {loc = Stack ss; typ = Int|Addr}, {loc = Reg rd} ->
              jit_ldr (emit_reg dst) (emit_stack src); 1
          | _ ->
              assert false
        end
    | Lop(Iconst_int n) ->
        emit_intconst i.res.(0) n
    | Lop(Iconst_float s) ->
        let bits = Int64.bits_of_float (float_of_string s) in
        let high_bits = Int64.to_nativeint (Int64.shift_right_logical bits 32)
        and low_bits = Int64.to_nativeint bits in
        if is_immediate low_bits && is_immediate high_bits then begin
          jit_mov (emit_reg i.res.(0)) (Immediate low_bits);
          jit_mov (emit_reg i.res.(1)) (Immediate high_bits);
          2
        end else begin
          let lbl = label_constant float_constants s 2 in
          jit_ldr (emit_reg i.res.(0)) (MemoryTag(jit_label_tag lbl, 0));
          jit_ldr (emit_reg i.res.(1)) (MemoryTag(jit_label_tag lbl, 4));
          2
        end
    | Lop(Iconst_symbol s) ->
        let lbl = label_constant symbol_constants s 1 in
        jit_ldr (emit_reg i.res.(0)) (MemoryTag(jit_label_tag lbl, 0)); 1
    | Lop(Icall_ind) ->
        jit_mov lr pc;
        jit_bx (emit_reg i.arg.(0));
        record_frame i.live i.dbg; 2
    | Lop(Icall_imm s) ->
        jit_bl_symbol s;
        record_frame i.live i.dbg; 1
    | Lop(Itailcall_ind) ->
        let n = frame_size() in
        if !contains_calls then
          jit_ldr lr (Memory(sp, Immediate(Nativeint.of_int (n - 4))));
        let ninstr = emit_stack_adjustment jit_add n in
        jit_bx (emit_reg i.arg.(0));
        2 + ninstr
    | Lop(Itailcall_imm s) ->
        if s = !function_name then begin
          jit_b_label !tailrec_entry_point; 1
        end else begin
          let n = frame_size() in
          if !contains_calls then
            jit_ldr lr (Memory(sp, Immediate(Nativeint.of_int (n - 4))));
          let ninstr = emit_stack_adjustment jit_add n in
          jit_b_symbol s;
          2 + ninstr
        end
    | Lop(Iextcall(s, alloc)) ->
        if alloc then begin
          let lbl = label_constant symbol_constants s 1 in
          jit_ldr r12 (MemoryTag(jit_label_tag lbl, 0));
          jit_bl_symbol "caml_c_call";
          record_frame i.live i.dbg; 2
        end else begin
          jit_bl_symbol s; 1
        end
    | Lop(Istackoffset n) ->
        assert (n mod 8 = 0);
        let ninstr =
          if n >= 0
          then emit_stack_adjustment jit_sub n
          else emit_stack_adjustment jit_add (-n) in
        stack_offset := !stack_offset + n;
        ninstr
    | Lop(Iload((Double | Double_u), addr)) ->
        let addr' = offset_addressing addr 4 in
        if i.res.(0).loc <> i.arg.(0).loc then begin
          jit_ldr (emit_reg i.res.(0)) (emit_addressing addr i.arg 0);
          jit_ldr (emit_reg i.res.(1)) (emit_addressing addr' i.arg 0)
        end else begin
          jit_ldr (emit_reg i.res.(1)) (emit_addressing addr' i.arg 0);
          jit_ldr (emit_reg i.res.(0)) (emit_addressing addr i.arg 0)
        end;
        2
    | Lop(Iload(size, addr)) ->
        let r = i.res.(0) in
        let instr =
          match size with
            Byte_unsigned -> jit_ldrb
          | Byte_signed -> jit_ldrsb
          | Sixteen_unsigned -> jit_ldrh
          | Sixteen_signed -> jit_ldrsh
          | _ (* 32-bit quantities *) -> jit_ldr in
        instr (emit_reg r) (emit_addressing addr i.arg 0);
        1
    | Lop(Istore((Double | Double_u), addr)) ->
        let addr' = offset_addressing addr 4 in
        jit_str (emit_reg i.arg.(0)) (emit_addressing addr i.arg 2);
        jit_str (emit_reg i.arg.(1)) (emit_addressing addr' i.arg 2);
        2
    | Lop(Istore(size, addr)) ->
        let r = i.arg.(0) in
        let instr =
          match size with
            Byte_unsigned
          | Byte_signed -> jit_strb
          | Sixteen_unsigned
          | Sixteen_signed -> jit_strh
          | _ (* 32-bit quantities *) -> jit_str in
        instr (emit_reg r) (emit_addressing addr i.arg 1);
        1
    | Lop(Ialloc n) ->
        if !fastcode_flag then begin
          let ni = emit_intconst (phys_reg 8 (*r12*)) (Nativeint.of_int n) in
          jit_sub alloc_ptr alloc_ptr r12;
          jit_cmp	alloc_ptr alloc_limit;
          jit_blcc_symbol "caml_call_gc";
          record_frame i.live i.dbg;
          jit_add (emit_reg i.res.(0)) alloc_ptr (Immediate 4n);
          4 + ni
        end else if n = 8 || n = 12 || n = 16 then begin
          let s = "caml_alloc" ^ (string_of_int ((n - 4) / 4)) in
          jit_bl_symbol s;
          record_frame i.live i.dbg;
          jit_add (emit_reg i.res.(0)) alloc_ptr (Immediate 4n); 2
        end else begin
          let ni = emit_intconst (phys_reg 8 (*r12*)) (Nativeint.of_int n) in
          jit_bl_symbol "caml_allocN";
          record_frame i.live i.dbg;
          jit_add (emit_reg i.res.(0)) alloc_ptr (Immediate 4n);
          2 + ni
        end
    | Lop(Iintop(Ilsl | Ilsr | Iasr as op)) ->
        let shift = shift_for_shift_operation op in
        jit_mov (emit_reg i.res.(0)) (Shift(emit_reg i.arg.(0), shift, emit_reg i.arg.(1))); 1
    | Lop(Iintop(Icomp cmp)) ->
        let cc = cc_for_comparison cmp in
        jit_cmp (emit_reg i.arg.(0)) (emit_reg i.arg.(1));
        jit_mov (emit_reg i.res.(0)) (Immediate 0n);
        jit_mov ~cc (emit_reg i.res.(0)) (Immediate 1n); 3
    | Lop(Iintop(Icheckbound)) ->
        jit_cmp (emit_reg i.arg.(0)) (emit_reg i.arg.(1));
        jit_blls_symbol "caml_ml_array_bound_error"; 2
    | Lop(Iintop op) ->
        (instr_for_intop op) (emit_reg i.res.(0)) (emit_reg i.arg.(0)) (emit_reg i.arg.(1)); 1
    | Lop(Iintop_imm(Idiv, n)) -> (* n is a power of 2 *)
        let l = Misc.log2 n in
        let r = i.res.(0) in
        jit_movs (emit_reg r) (emit_reg i.arg.(0));
        if n <= 256 then
          jit_addlt (emit_reg r) (emit_reg r) (Immediate(Nativeint.of_int(n-1)))
        else begin
          jit_addlt (emit_reg r) (emit_reg r) (Immediate(Nativeint.of_int n));
          jit_sublt (emit_reg r) (emit_reg r) (Immediate 1n)
        end;
        jit_mov (emit_reg r) (Shift(emit_reg r, ASR, Immediate(Nativeint.of_int l))); 4
    | Lop(Iintop_imm(Imod, n)) -> (* n is a power of 2 *)
        let l = Misc.log2 n in
        let a = i.arg.(0) in
        let r = i.res.(0) in
        let lbl = new_label() in
        jit_cmp (emit_reg a) (Immediate 0n);
        let lr = Nativeint.of_int (32 - l) in
        jit_mov (emit_reg r) (Shift(emit_reg a, LSL, Immediate lr));
        jit_mov (emit_reg r) (Shift(emit_reg r, LSR, Immediate lr));
        jit_bpl_label lbl;
        jit_cmp (emit_reg r) (Immediate 0n);
        jit_subne (emit_reg r) (emit_reg r) (Immediate(Nativeint.of_int n));
        jit_label lbl;
        6
    | Lop(Iintop_imm((Ilsl | Ilsr | Iasr as op), n)) ->
        let shift = shift_for_shift_operation op in
        jit_mov (emit_reg i.res.(0)) (Shift(emit_reg i.arg.(0), shift, Immediate(Nativeint.of_int n))); 1
    | Lop(Iintop_imm(Icomp cmp, n)) ->
        let cc = cc_for_comparison cmp in
        jit_cmp (emit_reg i.arg.(0)) (Immediate(Nativeint.of_int n));
        jit_mov (emit_reg i.res.(0)) (Immediate 0n);
        jit_mov ~cc (emit_reg i.res.(0)) (Immediate 1n); 3
    | Lop(Iintop_imm(Icheckbound, n)) ->
        jit_cmp (emit_reg i.arg.(0)) (Immediate(Nativeint.of_int n));
        jit_blls_symbol "caml_ml_array_bound_error"; 2
    | Lop(Iintop_imm(op, n)) ->
        (instr_for_intop op) (emit_reg i.res.(0)) (emit_reg i.arg.(0)) (Immediate(Nativeint.of_int n)); 1
    | Lop(Inegf) -> (* argument and result in (r0, r1) *)
        jit_eor r1 r1 (Immediate 0x80000000n); 1
    | Lop(Iabsf) -> (* argument and result in (r0, r1) *)
        jit_bic r1 r1 (Immediate 0x80000000n); 1
    | Lop(Ifloatofint | Iintoffloat | Iaddf | Isubf | Imulf | Idivf) ->
        assert false
    | Lop(Ispecific(Ishiftarith(op, shift))) ->
        (instr_for_shift_intop op)
          (emit_reg i.res.(0))
          (emit_reg i.arg.(0))
          (Shift(emit_reg i.arg.(1),
                 (if shift >= 0 then LSL else ASR),
                 Immediate(Nativeint.of_int(if shift >= 0
                                            then shift
                                            else (-shift))))); 1
    | Lop(Ispecific(Ishiftcheckbound shift)) ->
        jit_cmp (emit_reg i.arg.(1)) (Shift(emit_reg i.arg.(0), LSR, Immediate(Nativeint.of_int shift)));
        jit_blcs_symbol "caml_ml_array_bound_error"; 2
    | Lop(Ispecific(Irevsubimm n)) ->
        jit_rsb (emit_reg i.res.(0)) (emit_reg i.arg.(0)) (Immediate(Nativeint.of_int n)); 1
    | Lreloadretaddr ->
        let n = frame_size() in
        jit_ldr lr (Memory(sp, Immediate(Nativeint.of_int(n - 4)))); 1
    | Lreturn ->
        let ninstr = emit_stack_adjustment jit_add (frame_size()) in
        jit_bx lr;
        ninstr + 1
    | Llabel lbl ->
        jit_label lbl; 0
    | Lbranch lbl ->
        jit_b_label lbl; 1
    | Lcondbranch(tst, lbl) ->
        begin match tst with
          Itruetest ->
            jit_cmp (emit_reg i.arg.(0)) (Immediate 0n);
            jit_bne_label lbl
        | Ifalsetest ->
            jit_cmp (emit_reg i.arg.(0)) (Immediate 0n);
            jit_beq_label lbl
        | Iinttest cmp ->
            jit_cmp (emit_reg i.arg.(0)) (emit_reg i.arg.(1));
            let cc = cc_for_comparison cmp in
            jit_b_label ~cc lbl
        | Iinttest_imm(cmp, n) ->
            jit_cmp (emit_reg i.arg.(0)) (Immediate(Nativeint.of_int n));
            let cc = cc_for_comparison cmp in
            jit_b_label ~cc lbl
        | Ifloattest(cmp, neg) ->
            assert false
        | Ioddtest ->
            jit_tst (emit_reg i.arg.(0)) (Immediate 1n);
            jit_bne_label lbl
        | Ieventest ->
            jit_tst (emit_reg i.arg.(0)) (Immediate 1n);
            jit_beq_label lbl
        end;
        2
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
        jit_cmp (emit_reg i.arg.(0)) (Immediate 1n);
        begin match lbl0 with
          None -> ()
        | Some lbl -> jit_blt_label lbl
        end;
        begin match lbl1 with
          None -> ()
        | Some lbl -> jit_beq_label lbl
        end;
        begin match lbl2 with
          None -> ()
        | Some lbl -> jit_bgt_label lbl
        end;
        4
  | Lswitch jumptbl ->
        jit_ldr pc (Memory(pc, Shift(emit_reg i.arg.(0), LSL, Immediate 2n)));
        jit_mov r0 r0;      (* nop *)
        for i = 0 to Array.length jumptbl - 1 do
          jit_reloc (R_ABS_32(jit_label_tag jumptbl.(i)));
          jit_int32l 0l
        done;
        2 + Array.length jumptbl
    | Lsetuptrap lbl ->
        jit_bl_label lbl; 1
    | Lpushtrap ->
        stack_offset := !stack_offset + 8;
        jit_stmfd	~wb:true sp [trap_ptr; lr];
        jit_mov trap_ptr sp; 2
    | Lpoptrap ->
        jit_ldmfd ~wb:true sp [trap_ptr; lr];
        stack_offset := !stack_offset - 8; 1
    | Lraise ->
        jit_mov sp trap_ptr;
        jit_ldmfd ~wb:true sp [trap_ptr; pc]; 2

(* Emission of an instruction sequence *)

let no_fallthrough = function
    Lop(Itailcall_ind | Itailcall_imm _) -> true
  | Lreturn -> true
  | Lbranch _ -> true
  | Lswitch _ -> true
  | Lraise -> true
  | _ -> false

let rec emit_all ninstr i =
  if i.desc = Lend then () else begin
    let n = emit_instr i in
    let ninstr' = ninstr + n in
    let limit = 511 - !num_literals in
    if ninstr' >= limit - 64 && no_fallthrough i.desc then begin
      emit_constants();
      emit_all 0 i.next
    end else
    if ninstr' >= limit then begin
      let lbl = new_label() in
      jit_b_label lbl;
      emit_constants();
      jit_label lbl;
      emit_all 0 i.next
    end else
      emit_all ninstr' i.next
  end

(* Emission of a function declaration *)

let fundecl fundecl =
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := new_label();
  stack_offset := 0;
  Hashtbl.clear symbol_constants;
  Hashtbl.clear float_constants;
  jit_text();
  jit_align 0 4;
  jit_global fundecl.fun_name;
  jit_symbol fundecl.fun_name;
  let n = frame_size() in
  ignore(emit_stack_adjustment jit_sub n);
  if !contains_calls then
    jit_str lr (Memory(sp, Immediate(Nativeint.of_int (n - 4))));
  jit_label !tailrec_entry_point;
  emit_all 0 fundecl.fun_body;
  emit_constants()

(* Emission of data *)

let data = Jitaux.data

(* Beginning / end of an assembly file *)

let begin_assembly() =
  trampolines := [];
  Jitaux.begin_assembly()

let end_assembly() =
  (* Emit the trampolines *)
  jit_text();
  jit_align 0 4;
  List.iter (fun (sym, lbl) ->
               jit_label lbl;
               jit_ldr pc (Memory(pc, Immediate(-4n)));
               jit_reloc (R_ABS_32(jit_symbol_tag sym));
               jit_int32l 0l) !trampolines;
  Jitaux.end_assembly()
