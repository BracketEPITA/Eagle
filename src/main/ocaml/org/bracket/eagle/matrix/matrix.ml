type matrix = float array array

exception Invalid_dimension

let col_dim m = Array.length m
let row_dim m = Array.length m.(0)
let get m i j = m.(i).(j)

let copy a = Array.map (Array.copy) a
	
let unsafe_set m i j x = Array.unsafe_set (Array.get m i) j x
let unsafe_get m i j = Array.unsafe_get (Array.unsafe_get m i) j
	
let init col_dim row_dim f =
	let res = Array.create col_dim [||] in
	for i=0 to col_dim - 1 do
		Array.unsafe_set res i (Array.create row_dim (f i 0));
		for j=1 to row_dim - 1 do
			Array.unsafe_set (Array.unsafe_get res i) j (f i j)
		done;
	done;
	res
	
let apply f = function
	| [|[||]|] -> ()
	| m ->
	let col_dim = col_dim m in
		let row_dim = row_dim m in
		for i=0 to col_dim - 1 do
			let line = Array.unsafe_get m i in
			for j=0 to row_dim - 1 do
				Array.unsafe_set line j (f (Array.unsafe_get line j))
			done;
		done
	
let applyij (f : int -> int -> 'a -> 'a) = function
	| [|[||]|] -> ()
	| m ->
		let col_dim = col_dim m in
		let row_dim = row_dim m in
		for i=0 to col_dim - 1 do
			let line = Array.unsafe_get m i in
			for j=0 to row_dim - 1 do
				Array.unsafe_set line j (f i j (Array.unsafe_get line j))
			done;
		done
	
	
let random col_dim row_dim range =
	Random.self_init ();
	let float () =
		if Random.bool () then Random.float range
		else -. (Random.float range) in
	let res = Array.create col_dim [||] in
	for i=0 to col_dim - 1 do
		let new_line = Array.create row_dim (float()) in
		Array.unsafe_set res i new_line;
		for j=1 to row_dim - 1 do
			Array.unsafe_set new_line j (float())
		done
	done;
	res
	
let mul_vect m v =
	let vec_dim = Array.length v in
	let col_dim = col_dim m in
	let row_dim = row_dim m in
	if row_dim <> vec_dim then raise Invalid_dimension
	else (
		let res = Array.create col_dim 0. in
		for i=0 to col_dim - 1 do
			let sum = ref 0. in
			for j=0 to row_dim - 1 do
				sum := !sum +. (unsafe_get m i j) *. (Array.unsafe_get v j)
			done;
			Array.unsafe_set res i !sum
		done;
		res
	)
