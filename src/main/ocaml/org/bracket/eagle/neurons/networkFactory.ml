exception Wrong_init_sizes

class factory =
    object (self)
        val mutable range_w = 1.
        val mutable range_b = 1.
        val mutable layers  = ([] : int list)

        method with_weights w = range_w <- w; self
        method with_biases  b = range_b <- b; self
        method add_layer l = layers <- l::layers; self

        method build =
            let layers_sizes = Array.of_list (List.rev layers) in
            let input_size = layers_sizes.(0) in
            let output_size = layers_sizes.(Array.length layers_sizes - 1) in
	        Random.self_init ();
	        let init_bias i =
		        Random.float (2. *. range_b) -. range_b
            in
	        let layers_nb = Array.length layers_sizes in
	        if layers_sizes.(layers_nb - 1) <> output_size then
		        (raise Wrong_init_sizes);
	        let init_weights i =
		        let hidden_size = if i = 0 then 
                        input_size 
                    else 
                        layers_sizes.(i-1)
                in
		        Matrix.random layers_sizes.(i) hidden_size range_w in
            let init_bias i =
		        Array.init layers_sizes.(i) init_bias
            in
	        let weights = Array.init layers_nb init_weights in
	        let biases  = Array.init layers_nb init_bias    in
	        Network.(new basic_network {
                size_inputs  = input_size;
                size_outputs = output_size;
                init_weights = weights; 
                init_biases  = biases;
            })
    end
