open Core_kernel.Std
open Bap.Std
open Sparc_rtl


let make_reg name width = Var.create name (Type.imm width)

module Sparc64_vars = struct

  let gpr_bitwidth = 64

  let make_gpr name = make_reg name gpr_bitwidth

  let globals = "G"
  let outputs = "O"
  let locals = "L"
  let inputs = "I"

  let make_group pref start =
    List.init 8 ~f:(fun i ->
        let name = sprintf "%s%d" pref i in
        let alias = sprintf "R%d" (start + i) in
        make_gpr name, alias)

  let gprs = List.concat [
      make_group globals 0;
      make_group outputs 8;
      make_group locals 16;
      make_group inputs 24;
    ]

  let sp =fst @@  List.find_exn gprs ~f:(fun (_,a) -> String.equal a "O6")
  let fp = fst @@ List.find_exn gprs ~f:(fun (_,a) -> String.equal a "I6")

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

end

let of_vars = Map.map ~f:Exp.of_var
let of_vars_i = Map.map ~f:Exp.of_var

module Sparc64 = struct
  include Sparc64_vars
  module E = struct
    let gpr = of_vars gpr
    let gpri = of_vars_i gpri
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
