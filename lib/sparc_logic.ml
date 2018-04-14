open Sparc.Std

(* b8 12 20 10    or %o0, 16, %i4  *)
let or_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let tm = signed var cpu.word_width in
  RTL.[
    tm := im;
    rd := rs lor tm;
  ]

(* 9c 12 00 18    or %o0, %i0, %sp  *)
let or_rr cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs1 = unsigned cpu.reg ops.(1) in
  let rs2 = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs1 lor rs2;
  ]


(*   84 18 60 08    xor %g1, 8, %g2 *)
let xor_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let tm = signed var cpu.word_width in
  RTL.[
    tm := im;
    rd := rs lxor tm;
  ]

(* 96 1a 40 0a  xor %o1, %o2, %o3  *)
let xor_rr cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs1 = unsigned cpu.reg ops.(1) in
  let rs2 = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs1 lxor rs2;
  ]


(*  85 28 70 20  sllx %g1, 32, %g2
    85 28 60 16  sll %g1, 22, %g2 *)
let sl cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    rd := rs << im;
  ]

let sra_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let tm = signed var cpu.word_width in
  RTL.[
    tm := low word  rs;
    rd := tm >> im;
  ]

let srax_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    rd := rs >> im;
  ]

let srlx_ri cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    rd := rs >> im;
  ]

let () =
  "ORri"     >| or_ri;
  "ORrr"     >| or_rr;
  "XORri"   >| xor_ri;
  "XORrr"   >| xor_rr;
  "SLLri"     >| sl;
  "SLLXri"  >| sl;
  "SRLXri" >| srlx_ri;
  "SRAri"    >| sra_ri;
  "SRAXri"  >| srax_ri;
