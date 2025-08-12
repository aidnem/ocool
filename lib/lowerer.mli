type t

val init: t
val lower: Ast.program -> (Ir.program, string) result

val show : t -> string
