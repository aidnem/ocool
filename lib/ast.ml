(* type node = *)
(*     | Program of program *)
(*     | Expression of expression *)
(*     | Statement of statement *)
(*     [@@deriving show { with_path = false }, sexp] *)
type program =
    { outer_statements: outer_statement list }

and datatype =
    | Int
    | Float
    | Char
    | Pointer of datatype

and outer_statement =
    | FuncDef of func_def
    | Import of string

and func_def = 
    { name : identifier
    ; args: typed_identifier list
    ; body : block
    ; return : datatype }

and expression =
    | Identifier of identifier
    | Integer of int
    | Boolean of bool
    | String of string
    | Prefix of
        { operator : Token.t
        ; right: expression}
    | Infix of
        { left: expression
        ; operator :  Token.t
        ; right: expression }
    | If of
        { condition : expression
        ; consequence : block
        ; alternative : expression option }
    | Call of
        { fn : expression
        ; args: expression list }

and statement =
    | Let of
        { name : typed_identifier
        ; value : expression }
    | Return of expression
    | ExpressionStatement of expression
    | BlockStatement of block
    [@@deriving show]

and identifier = { identifier: string }

and typed_identifier =
    { name : string
    ; datatype: datatype }

and block = { block : statement list }

let new_program outer_statements = 
    { outer_statements };;
