type t =
    (* identifiers *)
    | Ident of string
    | String of string
    | Integer of int
    (* operators *)
    | Assign
    | Add
    | Sub
    | Mult
    | Div
    | LessThan
    | GreaterThan
    | Equal
    | NotEqual
    | Bang
    (* delimiters *)
    | Comma
    | Colon
    | Semicolon
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftBrace
    | RightBrace
    (* keywords *)
    | Import
    | Fn
    | Let
    | True
    | False
    | If
    | Else
    | Return
    [@@deriving show, eq]

let lookup_ident str =
    match str with
    | "fn" -> Fn
    | "let" -> Let
    | "true" -> True
    | "false" -> False
    | "if" -> If
    | "else" -> Else
    | "return" -> Return
    | non_keyword -> Ident non_keyword
;;
