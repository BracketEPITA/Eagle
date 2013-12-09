exception Wrong_dimension of string

let apply f v =
	for i=0 to (Array.length v) - 1 do
		Array.unsafe_set v i (f (Array.get v i))
	done

let applyi f v =
	for i=0 to (Array.length v) - 1 do
		Array.unsafe_set v i (f i (Array.get v i))
	done

let diff ~on:x y =
	let len = Array.length x in
	if (Array.length y) <> len then
		raise (Wrong_dimension("Vector : diff"));
	for i=0 to Pervasives.(-) len 1 do
		Array.unsafe_set x i ((Array.unsafe_get x i) -. (Array.unsafe_get y i))
	done
