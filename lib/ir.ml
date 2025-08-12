type ir_program =
    { functions : ir_function list }
    [@@deriving show]

and ir_function =
    { name : string
    ; instructions : instruction list }
    [@@deriving show]

and instruction =
    { result : string option
    ; action: action }
    [@@deriving show]

and action =
    | BinOp of ir_binary_expr
    | UnOp of ir_unary_expr
    | FunctionCall of ir_function_call
    [@@deriving show]

and ir_binary_expr =
    { left : value
    ; op : binary_op
    ; right : value }
    [@@deriving show]

and binary_op =
    | Add
    | Sub
    | Mult
    | Div
    | GT
    | LT
    | GTE
    | LTE
    [@@deriving show]

and ir_unary_expr =
    { op : unary_op
    ; value : value }
    [@@deriving show]

and unary_op =
    | Negate
    [@@deriving show]

and ir_function_call =
    { name : string
    ; args : value list }
    [@@deriving show]

and value =
    | Int of int
    | Float of float
    | Char of char
    | String of string
    | Var of string
    [@@deriving show]
