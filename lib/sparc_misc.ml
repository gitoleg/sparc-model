open Sparc.Std

let nop cpu ops = RTL.[]
let unimp cpu ops = RTL.[]

let sethi_i cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let im = unsigned imm ops.(1) in
  let tm = unsigned var cpu.word_width in
  let sh = unsigned const cpu.word_width 10 in
  RTL.[
    tm := im;
    rd := tm << sh;
  ]


let save_rr cpu ops = RTL.[]
let save_ri cpu ops = RTL.[]
let restore_rr cpu ops = RTL.[]
let restore_ri cpu ops = RTL.[]

let () =
  "NOP" >| nop;
  "UNIMP" >| unimp;
  "SETHIi" >| sethi_i;
  "SAVErr" >| save_rr;
  "SAVEri" >| save_ri;
  "RESTORErr" >| restore_rr;
  "RESTOREri" >| restore_ri
