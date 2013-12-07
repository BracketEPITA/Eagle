type layer = {
    size : int;
    has_bias : bool;
    activation : Network.ActivationFunction.func;
}

class factory =
    object
        val mutable layers = ([] : layer list)
        
        method add_layer (layer : layer) =
            layers <- layer::layers;

        method build = (
            let l = List.length in
            let length = l layers in
            let out_layers = Array.make length [||] in
            let layer_bias = Array.make length false in
            let layer_sizes = Array.make length 0 in
            let layer_activation = ref [] in
            let rec build_layers l0 i = match l0 with
                | e::li -> (
                        let size = e.size + if e.has_bias then 1 else 0 in
                        let layer = Array.make size 0. in
                        layer_bias.(length - i - 1) <- e.has_bias;
                        if e.has_bias then layer.(size - 1) <- 1.;
                        out_layers.(length - i - 1) <- layer;
                        layer_sizes.(length - i - 1) <- size;
                        layer_activation:=(e.activation)::(!layer_activation);
                        build_layers li (i + 1)
                    )
                | _    -> ()
            in build_layers layers 0;
            let weights = Array.init (length - 1) (fun i ->
                let l1 = layer_sizes.(i) and l2 = layer_sizes.(i + 1) in
                let s = l1 * (l2 - if layer_bias.(i + 1) then 1 else 0) in
                Array.init s (fun j ->
                    Random.float 1.
                )
            ) in
            let activations = Array.init (length - 1) (fun i ->
                match !layer_activation with
                    | act::li -> layer_activation := li; act
                    | [] -> failwith "Not enough activation functions"
            ) in
            
            (*print_string "layers\n";
            Array.iter (fun a -> 
                Array.iter (fun e -> Printf.printf "%f " e) a;
                print_newline();
            ) out_layers; print_newline();
            print_string "weights\n";
            Array.iter (fun a -> 
                Array.iter (fun e -> Printf.printf "%f " e) a;
                print_newline();
            ) weights; print_newline();
            Printf.printf "%d\n" (Array.length activations);
            *)

            new Network.basic_network {
                Network.layers = out_layers;
                Network.weights = weights;
                Network.activations = activations;
            }

        )


    end
