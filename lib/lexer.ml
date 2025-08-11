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

let rec next_token lexer =
    let lexer = skip_whitespace lexer in
    let open Token in
    match lexer.ch with
    | None -> lexer, None
    | Some ch ->
        let lexer, token =
            match ch with
            | ':' -> advance lexer, Colon
            | ';' -> advance lexer, Semicolon
            | '(' -> advance lexer, LeftParen
            | ')' -> advance lexer, RightParen
            | '[' -> advance lexer, LeftBracket
            | ']' -> advance lexer, RightBracket
            | '{' -> advance lexer, LeftBrace
            | '}' -> advance lexer, RightBrace
            | ',' -> advance lexer, Comma
            | '+' -> advance lexer, Add
            | '-' -> advance lexer, Sub
            | '*' -> advance lexer, Mult
            | '/' -> advance lexer, Div
            | '!' -> if_peeked lexer '=' ~default:Bang ~matched:NotEqual
            | '=' -> if_peeked lexer '=' ~default:Assign ~matched:Equal
            | '<' -> if_peeked lexer '=' ~default:LessThan ~matched:LessThanOrEqualTo
            | '>' -> if_peeked lexer '=' ~default:GreaterThan ~matched:GreaterThanOrEqualTo
            | '"' -> read_string lexer
            | ch when is_identifier ch -> read_identifier lexer
            | ch when is_number ch -> read_number lexer
            | ch -> Fmt.failwith "unknown char: %c" ch
        in
        lexer, Some token

and advance lexer =
    if lexer.position >= String.length lexer.input - 1
    then { lexer with ch = None }
    else (
        let position = lexer.position + 1 in
        { lexer with position; ch = Some (String.get lexer.input position)})

and peek_char lexer =
    if lexer.position >= String.length lexer.input - 1
    then None
    else Some (String.get lexer.input (lexer.position + 1))

and seek lexer condition =
    let rec loop lexer = if condition lexer.ch then loop @@ advance lexer else lexer in
    let lexer = loop lexer in
    lexer, lexer.position

and read_while lexer condition =
    let pos_start = lexer.position in
    let lexer, pos_end =
        seek lexer (fun ch ->
            match ch with
            | Some character -> condition character
            | None -> false)
    in
    lexer, String.sub lexer.input pos_start (pos_end - pos_start)

and is_identifier ch = (ch = '_' || is_alpha ch)
and is_alpha ch =
    match ch with
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false
and is_number ch =
    match ch with
    | '0' .. '9' -> true
    | _ -> false
and is_whitespace ch =
    match ch with
    | ' ' | '\t' | '\n' -> true
    | _ -> false

and read_string lexer =
    let rec read_string' lexer current_string is_escaped =
        if is_escaped then
            match lexer.ch with
            | Some 'n' -> read_string' (advance lexer) (current_string ^ "\n") false
            | Some 't' -> read_string' (advance lexer) (current_string ^ "\t") false
            | Some '\\' -> read_string' (advance lexer) (current_string ^ "\\") false
            | _ -> failwith "expected 'n', '\\', '\"', or 't' after '\\', found EOF"
        else
            match lexer.ch with
            | Some '"' -> advance lexer, ""
            | Some '\\' -> read_string' (advance lexer) current_string true
            | Some ch -> read_string' (advance lexer) (current_string ^ (String.make 1 ch)) false
            | None -> failwith "unclosed string literal, expected '\"' but found EOF"
    in
    let lexer, string_content = read_string' (advance lexer) "" false in
    lexer, Token.String string_content

and read_identifier lexer =
    let lexer, ident = read_while lexer is_identifier in
    lexer, Token.lookup_ident ident

and read_number lexer =
    let lexer, int = read_while lexer is_number in
    lexer, Token.Integer (int_of_string int)

and skip_whitespace lexer = 
    let lexer, _ =
        seek lexer (fun ch ->
            match ch with
            | Some ch -> is_whitespace ch
            | None -> false)
    in
    lexer

and if_peeked lexer ch ~default ~matched =
    let lexer, result =
        match peek_char lexer with
        | Some peeked when peeked = ch -> advance lexer, matched
        | _ -> lexer, default
    in
    advance lexer, result

(* and show lexer = *)
(*     "Lexer<\"" *)
(*     ^ lexer.input *)
(*     ^ "\", " *)
(*     ^ string_of_int lexer.position *)
(*     ^ ", '" *)
(*     ^ match lexer.ch with *)
(*         | Some ch -> String.make 1 ch *)
(*         | None -> "None" *)
(*     ^ "'>" *)
;;

