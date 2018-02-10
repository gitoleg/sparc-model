
open Sparc.Std

(* c2 2e a2 f0   stb  %g1, [ %i2 + 0x2f0 ] *)
let stb_ri cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  RTL.[
    cpu.store rd (rs + im) byte;
  ]

(* c0 28 40 02    stb %g0, [%g1+%g2]  *)
let stb_rr cpu ops =
  let rs1 = unsigned cpu.reg ops.(0) in
  let rs2 = unsigned cpu.reg ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  RTL.[
    cpu.store rd (rs1 + rs2) byte;
  ]

(* c2 27 a7 f3   st %g1, [%fp+2035]
   st ws renamed to stw in Sparc v9 *)
let stw_ri cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  RTL.[
    cpu.store rd (rs + im) word;
  ]

(* f0 77 a8 7f  stx  %i0, [ %fp + 0x87f ] *)
let stx_ri cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  RTL.[
    cpu.store rd (rs + im) doubleword;
  ]

(* c6 70 80 01  stx %g3, [%g2+%g1] *)
let stx_rr cpu ops =
  let rs1 = unsigned cpu.reg ops.(0) in
  let rs2 = unsigned cpu.reg ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  RTL.[
    cpu.store rd (rs1 + rs2) doubleword;
  ]

let () =
  "STBri" >| stb_ri;
  "STBrr" >| stb_rr;
  "STri"  >| stw_ri;
  "STXri" >| stx_ri;
  "STXrr" >| stx_rr;
