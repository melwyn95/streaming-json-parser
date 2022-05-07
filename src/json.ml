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
  | WHITE_SPACE of char (* \n, \t, \r, \s *)
  | STRING      of string
  | NUMBER      of float

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
- ridiculous idea: Make a smart contract / global contract in LIGO that will parse JSON

*)