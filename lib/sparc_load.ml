
open Sparc.Std

(* c2 08 40 00   ldub  [ %g1 ], %g1  *)
let ldub_rr cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs1 = unsigned cpu.reg ops.(1) in
  let rs2 = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := cpu.load (rs1 + rs2) byte;
  ]

(* f2 07 a8 7f   ld  [ %fp + 0x87f ], %i1
   ld ws renamed to lduw in Sparc v9 *)
let lduw_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let rs = signed cpu.reg ops.(2) in
  RTL.[
    rd := cpu.load (rs + im) word;
  ]

(*c2 5f 40 00    ldx  [ %i5 ], %g1  *)
let ldx_rr cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs1 = unsigned cpu.reg ops.(1) in
  let rs2 = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := cpu.load (rs1 + rs2) doubleword;
  ]

let () =
  "LDri"   >| lduw_ri;
  "LDUBrr" >| ldub_rr;
  "LDXrr"  >| ldx_rr;
