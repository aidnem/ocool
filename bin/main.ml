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
    | _ ->  Printf.eprintf "Error: more than one input file specified. This is not yet supporrted";
            Arg.usage speclist usage_msg;
            ()

let () =
    Arg.parse speclist anon_fun usage_msg;
    match !input_file with
    | "" -> Printf.eprintf "Error: no input file specified.";
            Arg.usage speclist usage_msg;
            ()
    | _ -> ()

