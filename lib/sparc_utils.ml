open Core_kernel.Std
open Bap.Std

let sparc_fail format =
  let fail str = failwith (sprintf "Sparc lifter fail: %s" str) in
  Printf.ksprintf fail format
