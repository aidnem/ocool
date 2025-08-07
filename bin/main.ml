module Token = Ocool.Token
module Lexer = Ocool.Lexer
module Parser = Ocool.Parser
module Ast = Ocool.Ast

let usage_msg = "ocool [-verbose] <source file> -o <output file>"

let verbose = ref false
let input_file = ref ""
let output_file = ref ""

let speclist =
    [
        ("-verbose", Arg.Set verbose, "Output debug information");
        ("-o", Arg.Set_string output_file, "Set output file name");
    ]

let anon_fun filename =
    match !input_file with
    | "" -> input_file := filename
    | _ ->  Printf.eprintf "Error: more than one input file specified. This is not yet supported";
            Arg.usage speclist usage_msg;
            ()

let read_file file =
    In_channel.with_open_bin file In_channel.input_all

let rec consume_lexer lexer =
    match Lexer.next_token lexer with
    | lexer, Some token ->
        print_string @@ Token.show token;
        print_string "\n";
        consume_lexer lexer
    | _, None -> ()

let print_program (program : Ast.program) : unit =
    print_string (Ast.show_program program);
    print_string "\n";;

let () =
    Arg.parse speclist anon_fun usage_msg;
    match !input_file with
    | "" -> Printf.eprintf "Error: no input file specified.";
            Arg.usage speclist usage_msg;
            ()
    | some_file ->
            let lexer = Lexer.init @@ read_file some_file in (
                print_string "Tokens: \n";
                consume_lexer lexer;
                match Parser.parse @@ Parser.init @@ Lexer.init @@ read_file some_file with
                | Ok(program) -> print_program program
                | Error e -> Printf.eprintf "%s\n" e
            )
