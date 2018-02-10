open Core_kernel.Std
open Bap.Std
open Sparc_rtl
open Sparc_dsl
open Sparc_model
open Sparc_utils

type cpu = {
  load       : exp -> bitwidth -> exp;
  store      : exp -> exp -> bitwidth -> rtl;
  jmp        : exp -> rtl;
  pc         : exp;
  word_width : bitwidth;

  (** registers  *)
  reg       : (op -> exp) ec; (** construct exp from register *)
  gpr       : int -> exp; (** general purpose registers 0..31 *)
}

let size_of_width x =
  let x = int_of_bitwidth x in
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> sparc_fail "invalid size: %d" x

let make_cpu endian memory =
  let open Sparc64 in
  let open E in
  let load addr width =
    let size = size_of_width width in
    Exp.load mem addr endian size in
  let store addr data width =
    let size = size_of_width width in
    store mem addr data endian size in
  let reg = reg (fun r -> Map.find_exn gpr (Reg.name r)) in
  let gpr n =
    try
      Map.find_exn gpri n
    with _ ->
      sparc_fail "GPR with number %d not found" n in
  let pc = Memory.min_addr memory |>
           Exp.of_word |>
           Exp.signed in
  let word_width = doubleword in {
    load;
    store;
    gpr;
    pc;
    jmp;
    reg;
    word_width;
  }
