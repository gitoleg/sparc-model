
open Sparc.Std

(* 9e 23 a0 30  sub %sp, 48, %o7*)
let sub_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[
    rd := rs - im;
  ]

(* 86 20 80 01  sub %g2, %g1, %g3  *)
let sub_rr cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs1 = unsigned cpu.reg ops.(1) in
  let rs2 = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs1 - rs2
  ]

let () =
  "SUBri" >| sub_ri;
  "SUBrr" >| sub_rr
