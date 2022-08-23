(* TODO: location on tokens maybe?? *)
(* TODO: don't read data just keep track of locations in file for DIGIT & CHAR *)
module Lexer = struct
  type token = 
  (* Single char tokens *)
    L_BRACE (* [ *)
  | R_BRACE (* ] *)
  | L_CURLY (* { *)
  | R_CURLY (* } *)
  | COMMA
  | COLON
  | DOUBLE_QUOTE
  | WHITE_SPACE of char (* \n, \t, \r, \s *)
  (* Multi char tokens *)
  | DIGIT of char
  | CHAR  of char

  let pp f = 
    function
      L_BRACE       -> Format.fprintf f "["
    | R_BRACE       -> Format.fprintf f "]"
    | L_CURLY       -> Format.fprintf f "{"
    | R_CURLY       -> Format.fprintf f "}"
    | COMMA         -> Format.fprintf f ","
    | COLON         -> Format.fprintf f ":"
    | DOUBLE_QUOTE  -> Format.fprintf f "\""
    | WHITE_SPACE w -> Format.fprintf f "%c" w
    | CHAR  c       -> Format.fprintf f "%c" c
    | DIGIT d       -> Format.fprintf f "%c" d

  type index = int

  type state = index * token

  (* TODO: should not need WHITE_SPACE, fix this *)
  let initial_state = 0, WHITE_SPACE ' '

  (* TODO: handle EOF ?? *)
  let lex : Buffer.t -> state -> state =
    fun b (pos, _) ->
      let c = Buffer.nth b pos in
      let token = 
        match c with
          '[' -> L_BRACE
        | ']' -> R_BRACE
        | '{' -> L_CURLY
        | '}' -> R_CURLY
        | ',' -> COMMA
        | ':' -> COLON
        | '"' -> DOUBLE_QUOTE
        | (' ' | '\n' | '\r' | '\t') as t -> WHITE_SPACE t
        | '0' .. '9' as d -> DIGIT d
        | _          as c -> CHAR  c
      in
      pos + 1, token
end

module PreParser = struct
  open Lexer
  (* TODO: maybe GADT ?? *)
  type token =
    L_BRACE (* [ *)
  | R_BRACE (* ] *)
  | L_CURLY (* { *)
  | R_CURLY (* } *)
  | COMMA
  | COLON
  | TRUE  of string
  | FALSE of string
  | NULL  of string
  | WHITE_SPACE of string 
  | STRING      of string
  | NUMBER      of string (* TODO: parse floats, exponents *)

  type status = Complete of token | Partial of token 
  type state = Single of status | Double of token * status

  let pp f = 
    function
      L_BRACE -> Format.fprintf f "["
    | R_BRACE -> Format.fprintf f "]"
    | L_CURLY -> Format.fprintf f "{"
    | R_CURLY -> Format.fprintf f "}"
    | COMMA   -> Format.fprintf f ","
    | COLON   -> Format.fprintf f ":"
    | TRUE _  -> Format.fprintf f "true"
    | FALSE _ -> Format.fprintf f "false"
    | NULL _  -> Format.fprintf f "null"
    | WHITE_SPACE w -> Format.fprintf f "%s" w
    | STRING s      -> Format.fprintf f "\"%s\"" s
    | NUMBER n      -> Format.fprintf f "%s" n

  let pp_status f =
    function
      Complete t -> Format.fprintf f "%a" pp t
    | Partial t  -> Format.fprintf f "Partial(%a)" pp t

  let initial_state = Complete (WHITE_SPACE "")

  let merge p q =
  match p, q with
    Partial STRING p, CHAR 't'           -> Single (Partial (STRING (p ^ (String.make 1 't'))))
  | Partial STRING p, CHAR 'f'           -> Single (Partial (STRING (p ^ (String.make 1 'f'))))
  | Partial STRING p, CHAR 'n'           -> Single (Partial (STRING (p ^ (String.make 1 'n'))))
  | Partial STRING p, CHAR q             -> Single (Partial (STRING (p ^ (String.make 1 q))))
  | Partial STRING p, DIGIT q            -> Single (Partial (STRING (p ^ (String.make 1 q))))
  | Partial STRING p, COMMA              -> Single (Partial (STRING (p ^ (String.make 1 ','))))
  | Partial STRING p, L_BRACE            -> Single (Partial (STRING (p ^ (String.make 1 '['))))
  | Partial STRING p, R_BRACE            -> Single (Partial (STRING (p ^ (String.make 1 ']'))))
  | Partial STRING p, L_CURLY            -> Single (Partial (STRING (p ^ (String.make 1 '{'))))
  | Partial STRING p, R_CURLY            -> Single (Partial (STRING (p ^ (String.make 1 '}'))))
  | Partial STRING p, COLON              -> Single (Partial (STRING (p ^ (String.make 1 ':'))))
  | Partial STRING p, WHITE_SPACE q      -> Single (Partial (STRING (p ^ (String.make 1 q))))

  | Partial NUMBER p, DIGIT q            -> Single (Partial (NUMBER (p ^ (String.make 1 q))))
  | Partial NUMBER p, CHAR '.'           -> Single (Partial (NUMBER (p ^ ".")))
  
  | Partial WHITE_SPACE p, WHITE_SPACE q -> Single (Partial (WHITE_SPACE (p ^ (String.make 1 q))))
  | Partial WHITE_SPACE w, DOUBLE_QUOTE  -> Double (WHITE_SPACE w, Partial (STRING ""))
  | Partial WHITE_SPACE w, DIGIT d       -> Double (WHITE_SPACE w, Partial (NUMBER (String.make 1 d)))
  
  | Partial STRING p, DOUBLE_QUOTE       -> Single (Complete (STRING p))
  
  | Partial WHITE_SPACE w, CHAR 't' -> Double (WHITE_SPACE w, Partial (TRUE "t"))
  | Complete _        , CHAR 't' -> Single (Partial (TRUE "t"))
  | Partial TRUE "t"  , CHAR 'r' -> Single (Partial (TRUE "tr"))
  | Partial TRUE "tr" , CHAR 'u' -> Single (Partial (TRUE "tru"))
  | Partial TRUE "tru", CHAR 'e' -> Single (Complete (TRUE "true"))

  | Partial WHITE_SPACE w, CHAR 'f' -> Double (WHITE_SPACE w, Partial (FALSE "f"))
  | Complete _          , CHAR 'f' -> Single (Partial (FALSE "f"))
  | Partial FALSE "f"   , CHAR 'a' -> Single (Partial (FALSE "fa"))
  | Partial FALSE "fa"  , CHAR 'l' -> Single (Partial (FALSE "fal"))
  | Partial FALSE "fal" , CHAR 's' -> Single (Partial (FALSE "fals"))
  | Partial FALSE "fals", CHAR 'e' -> Single (Complete (FALSE "false"))

  | Partial WHITE_SPACE w, CHAR 'n' -> Double (WHITE_SPACE w, Partial (NULL "n"))
  | Complete _        , CHAR 'n' -> Single (Partial (NULL "n"))
  | Partial NULL "n"  , CHAR 'u' -> Single (Partial (NULL "nu"))
  | Partial NULL "nu" , CHAR 'l' -> Single (Partial (NULL "nul"))
  | Partial NULL "nul", CHAR 'l' -> Single (Complete (NULL "null"))

  | Complete _, WHITE_SPACE w -> Single (Partial (WHITE_SPACE (String.make 1 w)))
  | Complete _, DIGIT d       -> Single (Partial (NUMBER (String.make 1 d)))
  | Complete _, DOUBLE_QUOTE  -> Single (Partial (STRING ""))

  | _ -> failwith (Format.asprintf "invalid: cannot merge `%a` `%a`" pp_status p Lexer.pp q) (* TODO: pretty printer for token & pre_parser_token*)

  let parse : Lexer.state -> status -> state =
  fun (_, token) p ->
  match token,p with
    WHITE_SPACE _, _ 
  | DIGIT _, _
  | DOUBLE_QUOTE, _ 
  | CHAR _, _ 
  | L_BRACE, Partial (STRING _)
  | R_BRACE, Partial (STRING _)
  | L_CURLY, Partial (STRING _)
  | R_CURLY, Partial (STRING _)
  | COLON, Partial (STRING _)
  | COMMA, Partial (STRING _) -> merge p token
  | COLON, _   -> Single (Complete COLON)
  | L_BRACE, _ -> Single (Complete L_BRACE)
  | L_CURLY, _ -> Single (Complete L_CURLY)
  | R_BRACE, _ -> 
    (match p with
      Partial p  -> Double (p, Complete R_BRACE)
    | Complete _ -> Single (Complete R_BRACE))
  | R_CURLY, _ -> 
    (match p with
      Partial p  -> Double (p, Complete R_CURLY)
    | Complete _ -> Single (Complete R_CURLY))
  | COMMA, _   -> 
    (match p with
      Partial p  -> Double (p, Complete COMMA)
    | Complete _ -> Single (Complete COMMA))

end

module Parser = struct
  open PreParser
  open Core

  module Generator = Base_quickcheck.Generator
  
  type str = string [@quickcheck.generator Generator.string_of Generator.char_alphanum]
  [@@deriving quickcheck, sexp_of, eq, ord]

  type t =
      Null
    | True
    | False
    | String of str 
    | Number of float [@quickcheck.weight 0.0] (* TODO: fix the case of only numbers *)
    | Object of (str * t) list
    | Array of t list
    [@@deriving quickcheck, sexp_of, eq, ord]

  module List = Caml.List
  module String = Caml.String
  module Stack = Caml.Stack

  let rec pp_t ppf t =
    match t with
      Null -> Format.fprintf ppf "null"
    | True -> Format.fprintf ppf "true"
    | False -> Format.fprintf ppf "false"
    | String s -> Format.fprintf ppf "\"%s\"" s
    | Number n -> Format.fprintf ppf "%f" n
    | Object rows -> 
      let rows = List.map (fun (k,v) -> Format.asprintf "\"%s\":%a" k pp_t v) rows in
      let rows = String.concat "," rows in
      Format.fprintf ppf "{%s}" rows
    | Array es -> 
      let es = String.concat "," (List.map (fun e -> Format.asprintf "%a" pp_t e) es) in
      Format.fprintf ppf "[%s]" es 


  type state = 
    Complete of t 
  | Object_start
  | Object_key of string
  | Object_key_colon of string
  | Object_row of string * t 
  | Array_start
  | Array_element of t

  type stack = state Stack.t

  let empty : unit -> stack = Stack.create

  let pp_stack ppf s = 
    Stack.iter (
      function 
        Complete t -> Format.fprintf ppf "Complete: %a\n" pp_t t
      | Object_start -> Format.fprintf ppf "Object_start\n" 
      | Object_key k -> Format.fprintf ppf "Object_key: %s\n" k
      | Object_key_colon k -> Format.fprintf ppf "Object_key_colon: %s\n" k
      | Object_row (k,t) -> Format.fprintf ppf "Object_row: %s : %a\n" k pp_t t
      | Array_start -> Format.fprintf ppf "Array_start\n"
      | Array_element t -> Format.fprintf ppf "Array_element: %a\n" pp_t t) s

  let step stack = 
    function 
      WHITE_SPACE _ -> stack
    | token -> 
    (
      (* Format.printf "%a\n" PreParser.pp token; *)
      let s_top = Stack.pop_opt stack in
      match s_top, token with
        None, TRUE _ -> Stack.push (Complete True) stack; stack
      | None, FALSE _ -> Stack.push (Complete False) stack; stack
      | None, STRING s -> Stack.push (Complete (String s)) stack; stack
      | None, NULL _ -> Stack.push (Complete (Null)) stack; stack
      (* TODO: only number doesn't work yet *)
      | None, NUMBER s -> Stack.push (Complete (Number (float_of_string s))) stack; stack

      | None, L_CURLY -> Stack.push Object_start stack; stack
      | Some Object_start, STRING s -> Stack.push (Object_key s) stack; stack
      | Some Object_start, R_CURLY -> 
        (match Stack.pop_opt stack with
          None -> Stack.push (Complete (Object [])) stack
        | Some Object_key_colon k -> Stack.push (Object_row (k,Object [])) stack
        | Some e -> Stack.push e stack; Stack.push (Array_element (Object [])) stack);
        stack
      | Some Object_key s, COLON -> Stack.push (Object_key_colon s) stack; stack
      | Some Object_key_colon s, TRUE _ -> Stack.push (Object_row (s,True)) stack; stack
      | Some Object_key_colon s, FALSE _ -> Stack.push (Object_row (s,False)) stack; stack
      | Some Object_key_colon s, STRING s' -> Stack.push (Object_row (s,String s')) stack; stack
      | Some Object_key_colon s, NULL _ -> Stack.push (Object_row (s,Null)) stack; stack
      | Some Object_key_colon s, NUMBER n -> Stack.push (Object_row (s,Number (float_of_string n))) stack; stack
      | Some Object_key_colon s, L_CURLY -> 
        Stack.push (Object_key_colon s) stack;
        Stack.push Object_start stack; stack
      | Some (Object_row _ as r), COMMA -> Stack.push r stack; stack
      | Some (Object_row _ as r), STRING s -> 
        Stack.push r stack;
        Stack.push (Object_key s) stack; stack
      | Some (Object_row (k',v')), R_CURLY ->
        let rec aux s =
          if Stack.is_empty s then [] else
          match Stack.pop s with
            Object_row (k,v) -> (k,v)::aux s
          | _ as e -> Stack.push e s; []
        in
        let kvs = aux stack in
        let kvs = (k',v')::kvs in
        let kvs = List.rev kvs in
        (match Stack.pop_opt stack with
          None -> Stack.push (Complete (Object kvs)) stack
        | Some Object_key_colon k -> Stack.push (Object_row (k,Object kvs)) stack
        | Some e -> Stack.push e stack; Stack.push (Array_element (Object kvs)) stack)
        ; stack

      | Some Object_key_colon s, L_BRACE ->
        Stack.push (Object_key_colon s) stack;
        Stack.push Array_start stack; stack

      | None, L_BRACE -> Stack.push Array_start stack; stack
      | Some Array_start, L_BRACE -> 
        Stack.push Array_start stack;
        Stack.push Array_start stack; stack
      | Some Array_start, R_BRACE -> 
        (match Stack.pop_opt stack with
          None -> Stack.push (Complete (Array [])) stack
        | Some (Object_key_colon k) -> Stack.push (Object_row (k, Array [])) stack
        | Some e -> 
          Stack.push e stack;
          Stack.push (Array_element ((Array []))) stack); 
          stack
      | Some Array_start, TRUE _ -> 
        Stack.push Array_start stack;
        Stack.push (Array_element True) stack; stack
      | Some Array_start, FALSE _ -> 
        Stack.push Array_start stack;
        Stack.push (Array_element False) stack; stack
      | Some Array_start, STRING s -> 
        Stack.push Array_start stack;
        Stack.push (Array_element (String s)) stack; stack
      | Some Array_start, NUMBER n -> 
        Stack.push Array_start stack;
        Stack.push (Array_element (Number (float_of_string n))) stack; stack
      | Some Array_start, NULL _ -> 
        Stack.push Array_start stack;
        Stack.push (Array_element Null) stack; stack

      | Some Array_start, L_CURLY ->
        Stack.push Array_start stack;
        Stack.push Object_start stack; stack

      | Some Array_element e, COMMA -> Stack.push (Array_element e) stack; stack
      | Some Array_element e, TRUE _ -> 
        Stack.push (Array_element e) stack;
        Stack.push (Array_element True) stack; stack
      | Some Array_element e, FALSE _ -> 
        Stack.push (Array_element e) stack;
        Stack.push (Array_element False) stack; stack
      | Some Array_element e, STRING s -> 
        Stack.push (Array_element e) stack;
        Stack.push (Array_element (String s)) stack; stack
      | Some Array_element e, NUMBER n -> 
        Stack.push (Array_element e) stack;
        Stack.push (Array_element (Number (float_of_string n))) stack; stack
      | Some Array_element e, NULL _ -> 
        Stack.push (Array_element e) stack;
        Stack.push (Array_element Null) stack; stack
      | Some Array_element e, L_BRACE ->
        Stack.push (Array_element e) stack;
        Stack.push (Array_start) stack; stack
      | Some Array_element e, R_BRACE ->
        let rec aux s =
          if Stack.is_empty s then [] else
          match Stack.pop s with
            Array_element e -> e::aux s
          | Array_start -> []
          | _ as e -> Stack.push e s; []
        in
        let es = e::aux stack in
        let es = List.rev es in
        (match Stack.pop_opt stack with
          None -> Stack.push (Complete (Array es)) stack
        | Some (Object_key_colon k) -> Stack.push (Object_row (k, Array es)) stack
        | Some e -> 
          Stack.push e stack;
          Stack.push (Array_element (Array es)) stack)
        ; stack

      | Some Array_element e, L_CURLY -> 
        Stack.push (Array_element e) stack;
        Stack.push Object_start stack; stack

      | _ -> failwith "not implemented")

  let rec of_yo_json (y : Yojson.Basic.t) =
    match y with
      `Null -> Null
    | `Bool true -> True
    | `Bool false -> False
    | `String s -> String s
    | `List t -> Array (List.map of_yo_json t)
    | `Assoc kvs -> Object (List.map (fun (k,v) -> k, of_yo_json v) kvs)
    | _ -> failwith "don't know ??"

  let of_string json =
    let module Buffer = Caml.Buffer in
    let buf = Buffer.of_seq @@ String.to_seq json in
    let len = Buffer.length buf in
    let rec aux len (l,p,p_stack) =
      (* Format.printf "%a\n" Parser.pp_stack p_stack; *)
      if len = 0 
      then (l,p,p_stack)
      else aux (len - 1) (
        let l = Lexer.lex buf l in
        let p, p_stack = 
          match PreParser.parse l p with 
            Single (Complete t as s) -> s, step p_stack t
          | Single (Partial _  as s) -> s, p_stack
          | Double (p,(Partial _ as s))  -> s, step p_stack p
          | Double (p,(Complete t as s)) -> s, step (step p_stack p) t
        in
        (l,p,p_stack))
    in
    let _,_,p_stack = aux len (Lexer.initial_state, PreParser.initial_state, empty ()) in
    match Stack.pop p_stack with
      Complete t -> t
    | _ -> failwith "parse error: ??"
end

let rec parse'' buf len (l, pp, p) =
  if len = 0 
  then (l, pp, p)
  else parse'' buf (len - 1) (
    let l = Lexer.lex buf l in
    let pp, p = 
      match PreParser.parse l pp with 
        Single (Complete t as s) -> s, Parser.step p t
      | Single (Partial _  as s) -> s, p
      | Double (tok,(Partial _ as s))  -> s, Parser.step p tok
      | Double (tok,(Complete t as s)) -> s, Parser.step (Parser.step p tok) t
    in
    (l , pp, p))

let buf_len = 1024

let parse' (refill : Buffer.t -> int) =
  let buf = Buffer.create buf_len in
  let init = Lexer.initial_state, PreParser.initial_state, Parser.empty () in
  let rec aux (l, pp, p) =
    let () = Buffer.clear buf in
    let len = refill buf in
    if len = 0 then
      if Stack.length p = 1 then Stack.pop p else failwith "parse error: invalid json." 
    else
      let (_,l), pp, p = parse'' buf len (l, pp, p) in
      aux ((0,l), pp, p)
  in
  let state = aux init in
  match state with
   Parser.Complete t -> t
  | _ -> failwith "parse error: incomplete json steam."

let parse_string s =
  let len = String.length s in
  let read = ref 0 in
  let refill buf =
    (* Format.printf "- %d / %d \n" !read len; *)
    let len = min (len - !read) buf_len in
    if len <= 0 then 0
    else 
      let () = Buffer.add_string buf (String.sub s !read len) in
      let () = read := !read + len in
      len
  in 
  parse' refill

(*
Idea: Make a streaming json parser, that can parse arbitrarily large json files.

- Specification: https://www.json.org/json-en.html

- Read 512 (or any fixed) number of bytes from a file
- Make a stream of tokens (tokens can be partial)
- Read the stream of tokens and create json tree (partial also)

- Need to merge two partial states
- Keep track of locations from source file for tokens (line number, start col index, end col index)

- Tokens: `{`, `}`, `[`, `]`, `:`, `,`, `true`, `false`, numbers (`float`), strings (`"`), whitespace, etc.

- Store numbers as floats
- See how to support unicode characters in string ?? (maybe use a library)

- Setup proper project using esy, lots of unit tests

- Single character tokens will not be interrupted (`{`, `}`, `[`, `]`, `:`, `,`)
- Multi character tokens will be interrupted need to partially lexed (`true`, `false`, numbers (`float`), strings (`"`), whitespace)

- Don't create copies of data while reading (maybe keep track of locations (start & end) from file)

- Good error messages ?? (maybe :D)

- benchmark with other libraries for json file > 1GB

- ridiculous idea: Write the partial parsing part in Coq later 
- ridiculous idea: Make a smart contract / global constant in LIGO that will parse JSON string

*)