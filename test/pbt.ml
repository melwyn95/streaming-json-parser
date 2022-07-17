open Core
module Format = Caml.Format

let%test_unit "Parse arbitrary JSON" =
  Quickcheck.test ~sexp_of:[%sexp_of: Json.Parser.t]
    [%quickcheck.generator: Json.Parser.t] ~f:(fun json ->
      let json = Format.asprintf "%a" Json.Parser.pp_t json in
      let j1 = try Json.Parser.of_yo_json (Yojson.Basic.from_string json) with _ -> Json.Parser.Null in
      let j2 = try Json.Parser.of_string json with _ -> Json.Parser.Null in
      if Caml.(j1 = Json.Parser.Null) then ()
      else [%test_eq: Json.Parser.t] j1 j2)