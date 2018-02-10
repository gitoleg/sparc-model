open Sparc.Std


let ba cpu ops =
  let disp = unsigned imm ops.(0) in
  let step = unsigned const word 4 in
  RTL.[
    jmp (cpu.pc + step * disp);
  ]


let () =
  "BPXCC" >| ba;
