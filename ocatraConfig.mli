type t
val create : ?port:int -> ?keepalive:float option -> ?processes:int -> unit -> t
val port : t -> int
val keepalive : t -> float option
val processes : t -> int
