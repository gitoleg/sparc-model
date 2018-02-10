open Core_kernel.Std
open Bap.Std

module Std = struct
  module RTL = Sparc_rtl
  include RTL.Infix
  include Sparc_dsl
  include Sparc_cpu


  let lifters = String.Table.create ()

  let register name lifter =
    Hashtbl.change lifters name ~f:(fun _ -> Some lifter)

  let (>|) = register

  let bil_of_rtl = RTL.bil_of_t

  let lift endian mem insn =
    let insn = Insn.of_basic insn in
    let insn_name = Insn.name insn in
    let cpu = make_cpu endian mem  in
    let lift lifter =
      try
        lifter cpu (Insn.ops insn) |>
        bil_of_rtl |>
        Result.return
      with
      | Failure str -> Error (Error.of_string str) in
    match Hashtbl.find lifters (Insn.name insn) with
    | None -> Or_error.errorf "unknown instruction %s" insn_name
    | Some lifter -> lift lifter


  module T64 = struct
    module CPU = Sparc_model.Cpu
    let lift = lift BigEndian
  end

  let () = register_target `sparcv9 (module T64)

end
