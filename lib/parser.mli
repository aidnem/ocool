type t

val init: Lexer.t -> t
val parse: t -> (Ast.program, string) result

val show : t -> string
