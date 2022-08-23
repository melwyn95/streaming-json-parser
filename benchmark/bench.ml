open Core
open Core_bench

let read f = 
  let s = ref "" in
  let ic = Caml.open_in f in
  let len = ref (Caml.Unix.stat f).st_size in
  let () = while !len > 0 do
    let buf = Bytes.create !len in
    let size = min 1024 !len in
    let len' = Caml.input ic buf 0 size in
    (* Format.printf "- %d %d %d\n" !len len' size; *)
    let () = s := !s ^ (Bytes.to_string (Caml.Bytes.sub buf 0 len')) in
    len := !len - len'
  done in
  !s

let small = read "benchmark/small.json"
let big = read "benchmark/big.json"

let sj = Yojson.Safe.to_string (Yojson.Safe.from_string small)
let sj' = Format.asprintf "%a" Json.Parser.pp_t (Json.parse_string small)
let () = assert String.(sj = sj')

let _ =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"YoJson small" 
      (fun () -> ignore (Yojson.Safe.from_string small));
    Bench.Test.create ~name:"MyJson small" 
      (fun () -> ignore (Json.parse_string small));
    Bench.Test.create ~name:"YoJson big" 
      (fun () -> ignore (Yojson.Safe.from_string big));
    Bench.Test.create ~name:"MyJson big" 
      (fun () -> ignore (Json.parse_string big));
  ])