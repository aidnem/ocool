type t =
    (* identifiers *)
    | Ident of string
    | String of string
    | Integer of int
    (* type names *)
    | NameInt
    | NameFloat
    | NameChar
    | NamePtr
    (* operators *)
    | Assign
    | Add
    | Sub
    | Mult
    | Div
    | LessThan
    | GreaterThan
    | LessThanOrEqualTo
    | GreaterThanOrEqualTo
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
    | "int" -> NameInt
    | "float" -> NameFloat
    | "ptr" -> NamePtr
    | "char" -> NameChar
    | non_keyword -> Ident non_keyword

let is_type_name token =
    match token with
    | NameInt | NameFloat | NamePtr -> true
    | _ -> false
