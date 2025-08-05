type t =
    { input: string
    ; position: int
    ; ch: char option }
    [@@deriving show]

let init input =
    if String.length input == 0
    then { input; position = 0; ch = None }
    else { input; position = 0; ch = Some (String.get input 0) }
;;

let rec next_token parser =
    let parser = skip_whitespace parser in
    let open Token in
    match parser.ch with
    | None -> parser, None
    | Some ch ->
        let peek = if_peeked parser in
        let parser, token =
            match ch with
            | ';' -> advance parser, Semicolon
            | '(' -> advance parser, LeftParen
            | ')' -> advance parser, RightParen
            | '[' -> advance parser, LeftBracket
            | ']' -> advance parser, RightBracket
            | '{' -> advance parser, LeftBrace
            | '}' -> advance parser, RightBrace
            | ',' -> advance parser, Comma
            | '+' -> advance parser, Add
            | '-' -> advance parser, Sub
            | '*' -> advance parser, Mult
            | '/' -> advance parser, Div

and read_identifier parser =
    let parser, ident = read_while parser is_identifier in
    parser, Token.lookup_ident ident

and read_number parser =
    let parser, ident = read_while parser is_number in
    parser, Token.Integer int

and skip_whitespace parser = 
    let parser, _ =
        seek parser (fun ch ->
            match ch with
            | Some ch -> Char.is_whitespace ch
            | None -> false)
    in
    parser

and if_peeked parser ch ~default ~matched =

