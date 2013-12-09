exception Wrong_input
exception Wrong_input_size

type vector = float array

type layer = {
	weights : Matrix.matrix;
	bias : vector;
}

module ActivationFunction = struct
    type func = {
        f  : (float -> float);
        f' : (float -> float);
    }

    let sigmoid = {
        f  = (fun x -> 1. /. (1. +. exp(-. x)));
        f' = (fun x -> x *. (1. -. x));
    }
end

type network_data = {
    size_inputs  : int;
    size_outputs : int;
    init_weights : Matrix.matrix array;
    init_biases  : vector array;
}

let input_matrix_size = 15

let shuffle a =
    Random.self_init();
    for i = Array.length a downto 1 do
	    let rand = Random.int i in
	    let temp = a.(rand) in
	    a.(rand) <- a.(i-1);
	    a.(i-1) <- temp
    done

let iter_random f a =
    let len = Array.length a in
    let indices = Array.init len (fun i -> i) in
    shuffle indices;
    Array.iter (fun i -> f a.(i)) indices

let array_map2 f a1 a2 =
	let len = Array.length a1 in
	if len <> Array.length a2 then
		raise (Invalid_argument "Array.map2");
	if len = 0 then [||]
	else (
		let r = Array.create len 
            (f (Array.unsafe_get a1 0) (Array.unsafe_get a2 0)) in
		for i = 1 to len - 1 do
			Array.unsafe_set r i 
                (f (Array.unsafe_get a1 i) (Array.unsafe_get a2 i))
		done;
		r
	)

class virtual network input_size output_size =
	object (self)
		method check_input_size (input : vector) =
			if (Array.length input) <> input_size then 
                (raise Wrong_input_size)
		method virtual feed : float array -> float array
	end

class basic_network (data : network_data) =
	object (self)
    	inherit network data.size_inputs data.size_outputs as network
	
		val layers_nb = Array.length data.init_weights
		val layers =
			array_map2
				(fun w b -> { weights = w; bias = b})
				data.init_weights data.init_biases
		
		method getLayersNb () = layers_nb
		
        method private feed_layer input layer = (
	        let potential = (Matrix.mul_vect layer.weights input) in
	        Vector.diff ~on:potential layer.bias;
	        Vector.apply ActivationFunction.(sigmoid.f) potential;
        	potential
        )

        method get_data = data

		method feed input =
			self#check_input_size input;
			Array.fold_left self#feed_layer input layers
		
		method private feedLayersResults input =
			network#check_input_size input;
			let results = Array.create layers_nb [||] in
			let temp = ref input in
			for i=0 to layers_nb - 1 do
				temp := self#feed_layer !temp layers.(i);
				results.(i) <- !temp
			done;
			results
		
		method private getResultsErrors input desired =
			let results = self#feedLayersResults input in
			let output = results.(layers_nb - 1) in
			let errors = Array.create layers_nb [||] in
			errors.(layers_nb-1) <-
				array_map2 (fun d o -> (d -. o) 
                    *. ActivationFunction.(sigmoid.f') (o)) desired output;
			for index=2 to layers_nb do
				let k = layers_nb - index in
				let weights = layers.(k+1).weights in
				let layer_size = Matrix.col_dim layers.(k).weights in
				errors.(k) <- Array.init layer_size (fun i ->
					let error = ref 0. in
					for j=0 to (Matrix.col_dim weights) - 1 do
						error := !error +. (Matrix.get weights j i) 
                            *. errors.(k+1).(j)
					done;
					error := !error 
                        *. (ActivationFunction.(sigmoid.f') results.(k).(i));
					!error
                )
			done;
			(results, errors)

        method train rate base = 
            iter_random (fun (i, d) -> self#train0 rate i d) base

		method private train0 rate input desired =
			let (results, errors) = self#getResultsErrors input desired in

		    (*Weights and bias rectifications*)
			for k=0 to layers_nb - 1 do
				let prev_results = if k=0 then input else results.(k-1) in
				
				Matrix.applyij
					(fun i j x -> 
                        x +. rate *. errors.(k).(i) *. prev_results.(j)
                    ) layers.(k).weights;
					
				Vector.applyi
					(fun i x -> x -. rate *. errors.(k).(i))
					layers.(k).bias
			done
		
		method copy () =
			let copy_layer l =
				{ weights = Matrix.copy l.weights;
				  bias = Array.copy l.bias; } in
			let new_layers = Array.map copy_layer layers in
			{< layers = new_layers >}
		
		end

