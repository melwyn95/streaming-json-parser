open Json

let json = "true"
let () = Driver.drive json

let json = "false"
let () = Driver.drive json

let json = "\"foo-bar\""
let () = Driver.drive json

let json = "null"
let () = Driver.drive json

(* let json = "3.142" *)
(* Driver.drive json *)

(* let json = "1234" *)
(* Driver.drive json *)

let json = "{}"
let () = Driver.drive json

let json = "{ \"foo\": \"string\", \"bar\": 1, \"baz\": true, \"tutu\": null }"
let () = Driver.drive json

let json = "{ \"a\" : { \"b\" : { \"c\" : { \"d\" : {} }}}}"
let () = Driver.drive json

let json = "{ \"a\" : [{ \"g\" : [3.142]}, {}, { \"f\" : 1}], \"b\" : false, \"c\" : [{}], \"d\" : null, \"e\": [1, true], \"i\": [], \"z\": {} }"
let () = Driver.drive json

let json = "[]"
let () = Driver.drive json

let json = "[true, false, null, \"foo\", 123.345]"
let () = Driver.drive json

let json = "[[[2, 3], [4, 5]], 1, [[[[[[[[]]]]]]]], null]"
let () = Driver.drive json

let json = "[{} , { \"a\" : 1} , { \"b\" : [1, true] } , { \"c\" : false , \"d\" : null}]"
let () = Driver.drive json

let json = "\"27]fXyN9,'KzbE5Z4mnt8A\""
let () = Driver.drive json