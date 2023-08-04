type 'a t =
  [ `Leaf of float
  | `Node of 'a node
  ]

and 'a node = {
  left : 'a t;
  right : 'a t;
  split : 'a;
}

type ft = float t

val learn : min_n:int -> max_depth:int -> (float * float) array -> ft
val infer : float -> ft -> float
val splits : ft -> float list
