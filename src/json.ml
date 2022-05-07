(* TODO: location on tokens maybe?? *)
(* TODO: don't read data just keep track of locations in file for DIGIT & CHAR *)
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

type index = int

type lexer_state = index * token 

let next_token : Buffer.t -> lexer_state -> lexer_state =
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

(* TODO: maybe GADT ?? *)
type pre_parse_tokens =
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

(* type 'a status = Complete of 'a | Partial of 'a *)

(* type pre_parser_state = pre_parse_tokens status *)

let merge_pre_parser_tokens p q =
  match p, q with
    STRING p, CHAR 't'           -> STRING (p ^ (String.make 1 't'))
  | STRING p, CHAR 'f'           -> STRING (p ^ (String.make 1 'f'))
  | STRING p, CHAR 'n'           -> STRING (p ^ (String.make 1 'n'))
  | STRING p, CHAR q             -> STRING (p ^ (String.make 1 q))
  | NUMBER p, DIGIT q            -> NUMBER (p ^ (String.make 1 q))
  | WHITE_SPACE p, WHITE_SPACE q -> WHITE_SPACE (p ^ (String.make 1 q))
  | STRING p, DOUBLE_QUOTE       -> STRING p
  | _       , DOUBLE_QUOTE       -> STRING ""
  | _         , CHAR 't' -> TRUE "t"
  | TRUE "t"  , CHAR 'r' -> TRUE "tr"
  | TRUE "tr" , CHAR 'u' -> TRUE "tru"
  | TRUE "tru", CHAR 'e' -> TRUE "true"
  | _           , CHAR 'f' -> FALSE "f"
  | FALSE "f"   , CHAR 'a' -> FALSE "fa"
  | FALSE "fa"  , CHAR 'l' -> FALSE "fal"
  | FALSE "fal" , CHAR 's' -> FALSE "fals"
  | FALSE "fals", CHAR 'e' -> FALSE "false"
  | _         , CHAR 'n' -> NULL "n"
  | NULL "n"  , CHAR 'u' -> NULL "nu"
  | NULL "nu" , CHAR 'l' -> NULL "nul"
  | NULL "nul", CHAR 'l' -> NULL "null"
  | _ -> failwith "invalid: cannot merge " (* TODO: pretty printer for token & pre_parser_token*)


let pre_parse : lexer_state -> pre_parse_tokens -> pre_parse_tokens =
  fun (_, token) p ->
    match token with
      L_BRACE -> L_BRACE
    | R_BRACE -> R_BRACE
    | L_CURLY -> L_CURLY
    | R_CURLY -> R_CURLY
    | COMMA   -> COMMA
    | COLON   -> COLON
    | WHITE_SPACE _ 
    | DIGIT _
    | DOUBLE_QUOTE 
    | CHAR _  -> merge_pre_parser_tokens p token 


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