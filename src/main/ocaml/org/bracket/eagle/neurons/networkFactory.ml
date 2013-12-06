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
            Random.self_init ();

            let l = List.length in
            let length = l layers in
            let out_layers = Array.make length [||] in
            let layer_bias = Array.make length false in
            let layer_sizes = Array.make length 0 in
            let layer_activation = ref [] in
            let rec build_layers l0 i = match l0 with
                | e::li -> (
                        layer_bias .(length - i - 1) <- e.has_bias;
                        out_layers .(length - i - 1) <- Array.make e.size 0.;
                        layer_sizes.(length - i - 1) <- e.size;
                        layer_activation:=(e.activation)::(!layer_activation);
                        build_layers li (i + 1)
                    )
                | _    -> ()
            in build_layers layers 0;
            let biases = Array.init (length - 1) (fun i ->
                if layer_bias.(i) then
                    Array.init layer_sizes.(i) (fun i -> 
                        Random.float 1.
                    )
                else [||]
            ) in
            let weights = Array.init (length - 1) (fun i ->
                let l1 = layer_sizes.(i) and l2 = layer_sizes.(i + 1) in
                Array.init (l1 * l2) (fun j ->
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
                Network.biases = biases;
                Network.activations = activations;
            }

        )


    end
