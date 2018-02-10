open Core_kernel.Std
open Bap.Std
open Sparc_rtl
open Sparc_dsl

let make_reg name width = Var.create name (Type.imm width)

module Sparc64_vars = struct

  let gpr_bitwidth = 64

  let make_gpr name = make_reg name gpr_bitwidth

  let make_group pref start =
    List.init 8 ~f:(fun i ->
        let name = sprintf "%s%d" pref i in
        let alias = sprintf "R%d" (start + i) in
        make_gpr name, alias)

  let gprs = List.concat [
      make_group "G" 0;   (* globals, G0 is always zero *)
      make_group "O" 8;   (* outputs *)
      make_group "L" 16;  (* locals  *)
      make_group "I" 24;  (* inputs  *)
    ]

  let sp =fst @@  List.find_exn
      gprs ~f:(fun (r,_) -> String.equal (Var.name r) "O6")
  let fp = fst @@ List.find_exn
      gprs ~f:(fun (r,_) -> String.equal (Var.name r) "I6")

  let gpr =
    let regs = List.fold gprs ~init:String.Map.empty
      ~f:(fun regs (reg,alias) ->
          let regs = Map.add regs (Var.name reg) reg in
          Map.add regs alias reg) in
    let regs = Map.add regs "SP" sp in
    Map.add regs "FP" fp

  let gpri = List.foldi gprs ~init:Int.Map.empty
      ~f:(fun i regs (reg,_) -> Map.add regs i reg)

  let mem = Var.create "mem" (Type.mem `r64 `r8)

  (* Condition Codes Register *)
  let ccr = Var.create "CCR" (Type.Imm 8)

end

let of_vars m = Map.map ~f:Exp.of_var m
let of_vars_i  m = Map.map ~f:Exp.of_var m

module Sparc64 = struct
  include Sparc64_vars
  module E = struct
    let gpr = of_vars gpr
    let gpri = of_vars_i gpri
    let ccr = Exp.of_var ccr
    let xcc = last ccr 4
    let ycc = first ccr 4
    let xcc_n = nth bit ccr 7
    let xcc_z = nth bit ccr 6
    let xcc_v = nth bit ccr 5
    let xcc_c = nth bit ccr 4
    let icc_n = nth bit ccr 3
    let icc_z = nth bit ccr 2
    let icc_v = nth bit ccr 1
    let icc_c = nth bit ccr 0
  end
end

module Cpu : CPU = struct
  include Sparc64

  let gpr = List.fold (Map.data gpr) ~init:Var.Set.empty ~f:Set.add

  let is = Var.same

  let flags = Var.Set.empty

  let is_reg r = Set.mem gpr (Var.base r)
  let is_flag r = Set.mem flags (Var.base r)
  let is_zf _ = false
  let is_cf _ = false
  let is_vf _ = false
  let is_nf _ = false
  let is_mem = is mem
  let is_sp = is sp
  let is_bp _ = false
  let zf = make_reg "ZF" 1
  let vf = make_reg "VF" 1
  let nf = make_reg "NF" 1
  let cf = make_reg "CF" 1
end
