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

let read_input file =
  let inc = In_channel.create file in
  let lines = In_channel.input_lines inc in
  In_channel.close inc;
  match lines with
  | [] -> eprintf "no input\n"; exit 1
  | fst :: _ ->
    let map = match String.prefix fst 2 with
      | "" | "\n" -> exit 0
      | "\\x" -> to_binary ~map:ident
      | "0x" ->  to_binary ~map:escape_0x
      | x -> to_binary ~map:prepend_slash_x in
    List.map lines ~f:(fun orig -> orig, map orig)

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

let get_insn arch bytes =
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = Addr.of_int ~width 0 in
  let mem = create_memory arch bytes addr in
  let dis = create_dis arch in
  match Dis.insn_of_mem dis mem with
  | Ok (mem, Some insn, _) -> mem, insn
  | _ ->  printf "disasm failed\n"; exit 1

let run file arch =
  let file = match file with
    | None -> eprintf "file is not set\n"; exit 1
    | Some x -> x in
let arch = match Arch.of_string arch with
    | None -> eprintf "unknown arch\n"; exit 1
    | Some arch -> arch in
  let insns = read_input file in
  let insns = List.map insns
      ~f:(fun (orig, bytes) -> orig, get_insn arch bytes) in
  List.iter insns (fun (orig, (mem,insn)) ->
      let name = Dis.Insn.name insn in
      match to_bil arch mem insn with
      | Error er ->
        let ers = Error.to_string_hum er in
        printf "%s failed: %s (%s)\n" name orig ers
      | Ok bil -> ())

module Cmdline = struct
  open Cmdliner

  let info =
    let doc = "sparc-test" in
    Term.info "sparc-test" ~doc

  let filename =
    let doc = "file with opcodes" in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

  let arch =
    let doc = "Target architecture" in
    Arg.(value & opt string "sparcv9" & info ["arch"] ~docv:"ARCH" ~doc)

  let run_t = Term.(const run $ filename $ arch), info

  let () =
    match Term.eval run_t ~catch:false with
    | `Ok opts -> ()
    | _ -> ()

end
