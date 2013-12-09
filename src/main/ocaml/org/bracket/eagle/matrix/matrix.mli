type matrix
exception Invalid_dimension

val col_dim : matrix -> int
val row_dim : matrix -> int
val get : matrix -> int -> int -> float
val copy : matrix -> matrix
val apply : (float -> float) -> matrix -> unit
val applyij : (int -> int -> float -> float) -> matrix -> unit
val random : int -> int -> float -> matrix
val mul_vect : matrix -> float array -> float array

