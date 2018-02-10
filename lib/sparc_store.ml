
open Sparc.Std

(* c2 2e a2 f0 stb  %g1, [ %i2 + 0x2f0 ] *)
let stb_ri cpu ops =
  let r1 = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let r2 = signed cpu.reg ops.(2) in
  RTL.[
    cpu.store r1 (r2 + im) byte;
  ]

(* c2 27 a7 f3   st %g1, [%fp+2035]
   st ws renamed to stw in Sparc v9 *)
let stw_ri cpu ops =
  let r1 = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let r2 = signed cpu.reg ops.(2) in
  RTL.[
    cpu.store r1 (r2 + im) word;
  ]

(* f0 77 a8 7f  stx  %i0, [ %fp + 0x87f ] *)
let stx_ri cpu ops =
  let r1 = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let r2 = signed cpu.reg ops.(2) in
  RTL.[
    cpu.store r1 (r2 + im) doubleword;
  ]

let () =
  "STBri" >| stb_ri;
  "STri"  >| stw_ri;
  "STXri" >| stx_ri;
