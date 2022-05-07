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
  | NUMBER      of string

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
  | Partial NUMBER p, DIGIT q            -> Single (Partial (NUMBER (p ^ (String.make 1 q))))
  
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
  match token with
    L_BRACE -> Single (Complete L_BRACE)
  | L_CURLY -> Single (Complete L_CURLY)
  | R_BRACE -> 
    (match p with
      Partial p  -> Double (p, Complete R_BRACE)
    | Complete _ -> Single (Complete R_BRACE))
  | R_CURLY -> 
    (match p with
      Partial p  -> Double (p, Complete R_CURLY)
    | Complete _ -> Single (Complete R_CURLY))
  | COMMA   -> 
    (match p with
      Partial p  -> Double (p, Complete COMMA)
    | Complete _ -> Single (Complete COMMA))
  | COLON   -> Single (Complete COLON)
  | WHITE_SPACE _ 
  | DIGIT _
  | DOUBLE_QUOTE 
  | CHAR _  -> merge p token

end

module Driver = struct
  let drive json =
    let buf = Buffer.of_seq @@ String.to_seq json in
    let len = Buffer.length buf in
    let rec aux len (l,p,xs) =
      if len = 0 
      then
        let xs = 
        (match p with
          PreParser.Complete t -> t::xs
        | Partial _            -> failwith "pre-parsing failure") in
        (l,p,List.rev xs)
      else aux (len - 1) (
        let l = Lexer.lex buf l in
        let p, xs = 
          match PreParser.parse l p with 
            Single (Complete t as s) -> s, t::xs
          | Single (Partial p  as s) -> s, xs
          | Double (p,q)             -> q, p::xs
        in
        (l,p,xs))
    in
    
    let _,p,tokens = aux len (Lexer.initial_state, PreParser.initial_state, []) in
    List.iter (fun t -> Format.printf "%a" PreParser.pp t) tokens
end

let json = "{ \"foo\": \"string\", \"bar\": 1, \"baz\": true }" ;;
Driver.drive json

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

- ridiculous idea: Write the partial parsing part in Coq later 
- ridiculous idea: Make a smart contract / global constant in LIGO that will parse JSON string

*)