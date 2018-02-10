open Sparc.Std

(* 02 c2 40 0b   brz,pn %o1, 11 *)
let bpznapn cpu ops =
  let rs1 = signed cpu.reg ops.(0) in
  let imm = signed imm ops.(1) in
  let stp = unsigned const word 4 in
  RTL.[
    when_ (rs1 = zero) [
      cpu.jmp (cpu.pc + stp * imm);
    ]
  ]

(* 0a c8 40 04  brnz    %g1, 4
   0a c0 40 04  brnz,pn %g1, 4 *)
let bpnznapt cpu ops =
  let rs1 = signed cpu.reg ops.(0) in
  let imm = signed imm ops.(1) in
  let stp = unsigned const word 4 in
  RTL.[
    when_ (rs1 <> zero) [
      cpu.jmp (cpu.pc + stp * imm);
    ]
  ]


let always = unsigned const byte 0b1000
let equal  = unsigned const byte 0b0001
let nequal = unsigned const byte 0b1001
let gequal = unsigned const byte 0b1101
let lequal = unsigned const byte 0b0100

(* 0a 6f ff f9    bcs %xcc, 524281
   10 6f ff a0    ba %xcc, 524192
   30 68 00 06    ba,a %xcc, 6
   02 80 00 04    be 4
   1a 60 00 0b    bcc,pn %xcc, 11
   08 60 00 0b 	  bleu,pn   %xcc, 11
   02 60 00 08    be,pn   %xcc, 8 *)
let bpxccnt cpu ops =
  let disp = signed imm ops.(0) in
  let cond = unsigned imm ops.(1) in
  let addr = unsigned var cpu.word_width in
  let step = unsigned const cpu.word_width 4 in
  RTL.[
    addr := cpu.pc + step * disp;
    when_ (cond = always) [
      jmp addr;
    ];
    when_ (cond = equal land cpu.xcc_z) [
      jmp addr;
    ];
    when_ (cond = nequal land (lnot cpu.xcc_z)) [
      jmp addr;
    ];
    when_ (cond = gequal land (lnot cpu.xcc_c)) [
      jmp addr;
    ];
    when_ (cond = lequal land (cpu.xcc_c lor cpu.xcc_z)) [
      jmp addr;
    ];
  ]

(* 12 40 00 2a   bne,pn	 %icc, 42
   06 4f ff f2   bl %icc, 524274 *)
let bpiccnt cpu ops =
  let disp = signed imm ops.(0) in
  let cond = unsigned imm ops.(1) in
  let addr = unsigned var cpu.word_width in
  let step = unsigned const cpu.word_width 4 in
  RTL.[
    addr := cpu.pc + step * disp;
    when_ (cond = always) [
      jmp addr;
    ];
    when_ (cond = equal land cpu.xcc_z) [
      jmp addr;
    ];
    when_ (cond = nequal land (lnot cpu.xcc_z)) [
      jmp addr;
    ];
    when_ (cond = gequal land (lnot cpu.xcc_c)) [
      jmp addr;
    ];
    when_ (cond = lequal land (cpu.xcc_c lor cpu.xcc_z)) [
      jmp addr;
    ];
  ]

let () =
  "BPXCC"    >| bpxccnt;
  "BCOND"    >| bpxccnt; (* has no info, xcc or icc should I use   *)
  "BPXCCNT"  >| bpxccnt;
  "BPXCCA"   >| bpxccnt;
  "BPZnapn"  >| bpznapn;
  "BPICC"    >| bpiccnt;
  "BPICCNT"  >| bpiccnt;
  "BPNZnapn" >| bpnznapt;
  "BPNZnapt" >| bpnznapt;
