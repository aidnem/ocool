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
    let parser = { lexer; current; peek } in
    parser

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
    let* parser = expect parser Token.LeftParen in
    let* parser, args = parse_arguments parser in
    let* parser, body = parse_block parser in
    Ok(parser, Ast.FuncDef { name; args ; body })
and parse_identifier (parser: t) : (t * Ast.identifier, string) result =
    match parser.current with
    | Some Token.Ident identifier -> Ok(advance parser, { identifier } )
    | _ -> Error "Expected identifier"
and expect parser tok =
    match parser.current with
    | Some(actual_tok) when tok == actual_tok -> Ok(advance parser)
    | _ -> Error (Printf.sprintf "Expected %s, found %s" (Token.show tok) (match parser.current with
                                                                          | Some(tok) -> Token.show tok
                                                                          | None -> "EOF"))
and parse_arguments parser =
    let parser = parser in
    let rec parse_arguments' parser args =
        match parser.current with
        | None -> Error "Expected identifier or ')', found EOF"
        | Some Token.Ident id -> 
                let parser = advance parser in
                (match parser.current with
                | Some Token.Comma -> parse_arguments' (advance parser) (id :: args)
                | Some Token.RightParen -> Ok(advance parser, [])
                | Some tok -> Error (Printf.sprintf "Expected identifier or ')' in function arguments, found %s" (Token.show tok))
                | None -> Error "Expected identifier or ')' in function arguments, found EOF")
        | Some tok -> Error (Printf.sprintf "Expected identifier or ')' in function arguments, found %s" (Token.show tok))
    in
    let* parser, args = parse_arguments' parser [] in
    Ok(parser, args)

and parse_block (parser : t) : (t * Ast.block, string) result =
    let parser = advance parser in
    let rec parse_block' (parser : t) (stmts : Ast.statement list) : (t * Ast.statement list, string) result =
        match parser.current with
        | None -> Error "Expected statement or '}', found EOF"
        | Some Token.RightBrace -> Ok((advance parser), [])
        | Some _ ->
            let* parser, statement = parse_statement parser in
                parse_block' parser (statement :: stmts)
    in
    let* parser, block = parse_block' parser [] in
    let block : Ast.block = { block } in
    Ok(parser, block)
and parse_statement parser =
    Ok(parser, Ast.Return(Ast.Integer 0))
