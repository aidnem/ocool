type t =
    { lexer : Lexer.t
    ; current : Token.t option
    ; peek : Token.t option }
    [@@deriving show]

type parse_error =
    { msg : string
    ; parser : t
    ; statements : Ast.statement list }
    [@@deriving show]

let init lexer =
    let parser = { lexer; current = None; peek = None } in
    let parser = advance parser in
    let parser = advance parser in
    parser
let rec parse parser =
    let rec parse' parser outer_statements =
        match parser.current with
        | Some _ ->
                (match parse_outer_statement parser with
                    | Ok (parser, stmt) -> parse' (advance parser) (stmt :: outer_statements)
                    | Error msg -> err parser msg statements)
        | None -> Ok (parser, List.rev outer_statements)
    in
    let* _, outer_statements = parse' parser [] in
    Ok (Ast.Program { outer_statements })

and parse_outer_statement parser =
    match parser.current with
    | Some Token.Import -> parse_import parser
    | Some Token.Fn -> parse_function parser
    | Some _ -> error "expected 'import' or 'fn'"
    | None -> error "no more tokens"

and parse_import parser =
    let* parser, name = parse_identifier parser in
    Ok (parser, Ast.Import { name })

and parse_

