type t

val init: string -> t

val next_token : t -> t * Token.t option

val show : t -> string
