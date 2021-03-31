module type Player = sig
  type player

  val add_resource : (*resource*) int -> player -> player
end
