open Json

let print_parsed s = Format.printf "%a\n" Parser.pp_t s

let json = "true"
let () = print_parsed (parse_string json)

let json = "false"
let () = print_parsed (parse_string json)

let json = "\"foo-bar\""
let () = print_parsed (parse_string json)

let json = "null"
let () = print_parsed (parse_string json)

(* let json = "3.142" *)
(* print_parsed (parse_string json) *)

(* let json = "1234" *)
(* print_parsed (parse_string json) *)

let json = "{}"
let () = print_parsed (parse_string json)

let json = "{ \"foo\": \"string\", \"bar\": 1, \"baz\": true, \"tutu\": null }"
let () = print_parsed (parse_string json)

let json = "{ \"a\" : { \"b\" : { \"c\" : { \"d\" : {} }}}}"
let () = print_parsed (parse_string json)

let json = "{ \"a\" : [{ \"g\" : [3.142]}, {}, { \"f\" : 1}], \"b\" : false, \"c\" : [{}], \"d\" : null, \"e\": [1, true], \"i\": [], \"z\": {} }"
let () = print_parsed (parse_string json)

let json = "[]"
let () = print_parsed (parse_string json)

let json = "[true, false, null, \"foo\", 123.345]"
let () = print_parsed (parse_string json)

let json = "[[[2, 3], [4, 5]], 1, [[[[[[[[]]]]]]]], null]"
let () = print_parsed (parse_string json)

let json = "[{} , { \"a\" : 1} , { \"b\" : [1, true] } , { \"c\" : false , \"d\" : null}]"
let () = print_parsed (parse_string json)

let json = "\"27]fXyN9,'KzbE5Z4mnt8A\""
let () = print_parsed (parse_string json)