open Sparc.Std

(* 94 03 a8 b7  add %sp, 2231, %o2 *)
let add_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[
    rd := rs + im;
  ]

(* 82 05 c0 01   add %l7, %g1, %g1 *)
let add_rr cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs1 = unsigned cpu.reg ops.(1) in
  let rs2 = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs1 + rs2
  ]

let () =
  "ADDri" >| add_ri;
  "ADDrr" >| add_rr
