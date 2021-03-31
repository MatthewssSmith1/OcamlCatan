type t

val add_resource : (*resource*) int -> t -> t

val remove_resource : (*resource*) int -> t -> t

val check_resource : (*resource*) int -> t -> t

val add_port : (*port*) t -> t

val make_player : t
