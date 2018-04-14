open Sparc.Std

let nop cpu ops = RTL.[]

let sethi_i cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let im = unsigned imm ops.(1) in
  let tm = unsigned var cpu.word_width in
  let sh = unsigned const cpu.word_width 10 in
  RTL.[
    tm := im;
    rd := tm << sh;
  ]

let () =
  "NOP" >| nop;
  "SETHIi" >| sethi_i;
