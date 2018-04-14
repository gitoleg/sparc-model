open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std

module Dis = Disasm_expert.Basic
open Sparc

let () =
  match Plugins.load () |> Result.all with
  | Ok plugins -> ()
  | Error (path, er) ->
    Printf.eprintf "failed to load plugin from %s: %s"
      path (Error.to_string_hum er)

let escape_0x =
  String.substr_replace_all ~pattern:"0x" ~with_:"\\x"

let prepend_slash_x x = "\\x" ^ x

(** [to_binary ?escape s] make a binary string from ascii
    representation, (e.g., "\x01\x02..."). Apply optional
    escape function for each byte *)
let to_binary ?(map=ident) s =
  let seps = [' '; ','; ';'] in
  let separated = List.exists seps ~f:(String.mem s) in
  let bytes = if separated
    then String.split_on_chars ~on:seps s
    else List.init (String.length s / 2) ~f:(fun n ->
        String.slice s (n*2) (n*2+2)) in
  try bytes |> List.map ~f:map |> String.concat |> Scanf.unescaped
  with Scanf.Scan_failure _ ->
    eprintf "bad user input\n"; exit 1

let read_input () =
  let input = In_channel.input_line In_channel.stdin in
  match input with
  | None -> eprintf "no input\n"; exit 1
  | Some input -> match String.prefix input 2 with
    | "" | "\n" -> exit 0
    | "\\x" -> to_binary input
    | "0x" ->  to_binary ~map:escape_0x input
    | x -> to_binary ~map:prepend_slash_x input

let create_dis arch =
  Dis.create ~backend:"llvm" (Arch.to_string arch) |>
  Or_error.ok_exn |>
  Dis.store_kinds |>
  Dis.store_asm

let create_memory arch s addr =
  let endian = Arch.endian arch in
  Memory.create endian addr @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error e ->
    eprintf "something went wrong\n"; exit 1

let to_bil arch mem insn =
  let module T = (val (target_of_arch arch)) in
  T.lift mem insn

let print_insn insn =
  let str_of_op = function
    | Op.Imm imm ->
      sprintf "imm %s;" (Imm.to_string imm)
    | Op.Reg reg ->
      sprintf "reg %s;" (Reg.to_string reg)
    | Op.Fmm fmm ->
      sprintf "fmm %s;" (Fmm.to_string fmm) in
  let name = Dis.Insn.name insn in
  let ops = Dis.Insn.ops insn in
  let asm = Dis.Insn.asm insn in
  let ops = Array.fold ~init:"" ops
      ~f:(fun s o -> sprintf "%s%s " s @@ str_of_op o) in
  printf "%s %s, %s\n" name ops asm

let get_insn arch bytes =
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = Addr.of_int ~width 0 in
  let mem = create_memory arch bytes addr in
  let dis = create_dis arch in
  match Dis.insn_of_mem dis mem with
  | Ok (mem, Some insn, _) -> mem, insn
  | _ -> eprintf "disasm failed\n"; exit 1

let string_of_bytes bytes =
  String.fold ~init:"" ~f:(fun acc b ->
      sprintf "%s%02X " acc (Char.to_int b)) bytes

let run arch =
  let arch = match Arch.of_string arch with
    | None -> eprintf "unknown arch\n"; exit 1
    | Some arch -> arch in
  let bytes = read_input () in
  let mem, insn = get_insn arch bytes in
  print_insn insn;
  match to_bil arch mem insn with
  | Error er ->
    let er = Error.to_string_hum er in
    let bytes = string_of_bytes bytes in
    eprintf "bil was not obtained: %s for %s\n" er bytes; exit 1
  | Ok bil ->
    printf "%s\n" (Bil.to_string bil)

module Cmdline = struct
  open Cmdliner

  let info =
    let doc = "sparc-mc" in
    Term.info "sparc-mc" ~doc

  let arch =
    let doc = "Target architecture" in
    Arg.(value & opt string "sparcv9" & info ["arch"] ~docv:"ARCH" ~doc)

  let run_t = Term.(const run $ arch), info

  let () =
    match Term.eval run_t ~catch:false with
    | `Ok opts -> ()
    | _ -> ()

end
