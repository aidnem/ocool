type t =
    { lexer : Lexer.t
    ; current : Token.t option
    ; peek : Token.t option }
    [@@deriving show]

let ( let* ) x f = Result.bind x f

type parse_error =
    { msg : string
    ; parser : t
    ; statements : Ast.statement list }
    [@@deriving show]

let advance parser =
    let current = parser.peek in
    let lexer, peek = Lexer.next_token parser.lexer in
    { lexer; current; peek }

let init lexer =
    let parser = { lexer; current = None; peek = None } in
    let parser = advance parser in
    let parser = advance parser in
    parser
let rec parse (parser : t) : (Ast.program, string) result =
    let rec parse' parser outer_statements =
        match parser.current with
        | Some _ ->
                (match parse_outer_statement parser with
                    | Ok (parser, stmt) -> parse' (advance parser) (stmt :: outer_statements)
                    | Error msg -> Error msg)
        | None -> Ok (parser, List.rev outer_statements)
    in
    match parse' parser [] with
    | Error e -> Error e
    | Ok (_, stmts) -> Ok (Ast.new_program stmts)
and parse_outer_statement (parser : t) : (t * Ast.outer_statement, string) result =
    match parser.current with
    | Some Token.Import -> parse_import parser
    | Some Token.Fn -> parse_function parser
    | _ -> Error "Expected 'import' or 'fn' keywords."
and parse_import parser =
    let parser = advance parser in
    let* parser, ident = parse_identifier parser in
    Ok(parser, Ast.Import ident.identifier)
and parse_function (parser : t) : (t * Ast.outer_statement, string) result =
    let parser = advance parser in
    let* parser, name = parse_identifier parser in
    (* let* parser = expect parser Token.LeftParen in *)
    (* let* parser, args = parse_arguments parser in *)
    (* let* parser = expect parser Token.RightParen in *)
    (* let* parser, body = parse_block parser in *)
    Ok(parser, Ast.FuncDef { name, [], { block=[] }})
and parse_identifier (parser: t) : (t * Ast.identifier, string) result =
    match parser.current with
    | Some Token.Ident identifier -> Ok(advance parser, { identifier } )
    | _ -> Error "Expected identifier"
and expect parser tok =
    match parser.current with
    | Some(tok) -> advance parser
    | _ -> Error (Fmt.fmt "Expected %s, found %s" show tok show parser.current)
