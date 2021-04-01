type t

val learn : min_n:int -> max_depth:int -> (float * float) array -> t
val infer : float -> t -> float

val json_of_t : t -> string
val t_of_json : string -> t

val biniou_of_t : t -> string
val t_of_biniou : string -> t
