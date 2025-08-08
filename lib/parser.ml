type t =
    { lexer : Lexer.t
    ; current : Token.t option
    ; peek : Token.t option }
    [@@deriving show]

let ( let* ) x f = Result.bind x f
let ( >>= ) x f = Result.bind x f

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

let is_infix_operator token =
    match token with
    | Token.Add -> true
    | Token.Sub -> true
    | Token.Mult -> true
    | Token.Div -> true
    | _ -> false
(**)
(* let binding_power token left = *)
(*     let res = match token with *)
(*         | Token.Add -> 1 *)
(*         | Token.Sub -> 1 *)
(*         | Token.Mult -> 2 *)
(*         | Token.Div -> 2 *)
(*         | _ -> 0 *)
(*     in *)
(*     res * 2 + if left then 1 else 0 *)

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
    let* parser = expect Token.LeftParen parser in
    let* parser, args = parse_arguments parser in
    (* let args = List.map (fun a -> { identifier = a } : string -> Ast.identifier) args in *)
    let* parser, body = parse_block parser in
    Ok(parser, Ast.FuncDef { name; args ; body })
and parse_identifier (parser: t) : (t * Ast.identifier, string) result =
    match parser.current with
    | Some Token.Ident identifier -> Ok(advance parser, { identifier } )
    | _ -> Error "Expected identifier"
and expect tok parser =
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
                let id : Ast.identifier = { identifier = id } in
                (match parser.current with
                | Some Token.Comma -> parse_arguments' (advance parser) (id :: args)
                | Some Token.RightParen -> Ok(advance parser, id :: args |> List.rev)
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
        | Some Token.RightBrace -> Ok((advance parser), List.rev stmts)
        | Some _ ->
            let* parser, statement = parse_statement parser in
                parse_block' parser (statement :: stmts)
    in
    let* parser, block = parse_block' parser [] in
    let block : Ast.block = { block } in
    Ok(parser, block)
and parse_statement parser =
    match parser.current with
    | Some Token.Return ->
            let* parser, return_expr = parser |> advance |> parse_expression in
            Ok(parser, Ast.Return(return_expr))
    | Some Token.Let ->
            parse_let_statement parser
    | Some tok -> Error (Printf.sprintf "Expected statement (e.g. return ...), found %s" (Token.show tok))
    | None -> Error "Expected statement, found EOF"
and parse_let_statement parser =
    let parser = advance parser in (* move past 'let' keyword *)
    let* parser, name = parse_identifier parser in
    let* parser, value = parser |> expect Token.Assign >>= parse_expression in
    Ok(parser, Ast.Let { name; value })
and parse_expression parser =
    match parser.current with
    | Some Token.Sub ->
            let* parser, right = parser |> advance |> parse_expression in
            Ok(parser, Ast.Prefix { operator = Token.Sub; right })
    | _ ->
            let* parser, lhs = parser |> parse_atom in
            let parser = parser |> advance in
            match parser.current with
                | Some token when is_infix_operator token ->
                    (let operator = token in
                    let parser = parser |> advance in
                    let* parser, rhs = parse_atom parser in
                    Ok(parser, Ast.Infix { left=lhs; operator; right=rhs }))
                | _ -> Ok(parser, lhs)

and parse_atom parser =
    match parser.current with
    | Some Token.Integer value ->
            Ok(advance parser, Ast.Integer value )
    | Some Token.Ident name ->
            if parser.current == Some Token.LeftParen then
                parse_function_call parser
            else
                Ok(advance parser, Ast.Identifier { identifier = name })
    | Some tok -> Error (Printf.sprintf "Expected a valid atom (-[number], [number], [number + number], function_call()), but found %s" (Token.show tok))
    | _ -> Error "Expected atom or expression, found EOF"

and parse_call_arguments parser =
    let parser = parser in
    let rec parse_call_arguments' parser args =
        let* parser, arg = parser |> parse_expression in
        match parser.current with
        | Some Token.Comma -> parse_call_arguments' (advance parser) (arg :: args)
        | Some Token.RightParen -> Ok(advance parser, arg :: args |> List.rev)
        | Some tok -> Error (Printf.sprintf "Expected expression or ')' in function arguments, found %s" (Token.show tok))
        | None -> Error "Expected expression or ')' in function arguments, found EOF"
    in
    let* parser, args = parse_call_arguments' parser [] in
    Ok(parser, args)

and parse_function_call parser =
    match parser.current, parser.peek with
    | Some Token.Ident name, Some Token.LeftParen ->
        let parser = parser |> advance |> advance in
        let* parser, args = parse_call_arguments parser in
        Ok(parser, Ast.Call { fn = Ast.Identifier { identifier = name }; args })
    | _ -> Error "unreachable, parse_function_call must only be called on an identifier followed by a ("
