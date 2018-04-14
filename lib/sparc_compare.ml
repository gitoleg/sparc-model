open Sparc.Std

(* 80 a0 60 0e  cmp %g1, 14 *)
let cmp_ri cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let low_rs = unsigned var word in
  let low_im = unsigned var word in
  let low_sb = unsigned var word in
  RTL.[
    low_rs := low word rs;
    low_im := low word im;
    low_sb := low_rs - low_im;
    cpu.icc_v := msb low_rs <> msb low_im
                               land msb low_rs <> msb low_sb;
    cpu.xcc_v := msb rs <> msb im
                               land msb rs <> msb (rs - im);
  ]

let cmp_rr cpu ops =
  let rs1 = unsigned cpu.reg ops.(0) in
  let rs2 = unsigned cpu.reg ops.(1) in
  let low_rs1 = unsigned var word in
  let low_rs2 = unsigned var word in
  let low_sb = unsigned var word in
  RTL.[
    low_rs1 := low word rs1;
    low_rs2 := low word rs2;
    low_sb := low_rs1 - low_rs2;
    cpu.icc_v := msb low_rs1 <> msb low_rs2
                               land msb low_rs1 <> msb low_sb;
    cpu.xcc_v := msb rs1 <> msb rs2
                               land msb rs1 <> msb (rs1 - rs2);
  ]


let () =
  "CMPri" >| cmp_ri;
  "CMPrr" >| cmp_rr
