open Math.Universe

module ActivationFunction = struct
    type func = (float -> float)
    let linear (x : float) = x
    let sigmoid x = 1. /. (1. +. exp(-. x))
    let tanh = ()
end

type network_data = {
    layers  : float array array;
    weights : float array array;
    activations : ActivationFunction.func array;
}

class virtual network (data : network_data) =
    object (this)
        method get_data = data
        method virtual get_layer : int -> float array
        method virtual set_values : float array -> float array
    end

class basic_network (data : network_data) = 
    object (this)
        inherit network data

        val mutable training = (
            fun x y z -> 0. : float array -> float array -> network -> float
        )

        method get_layer i = (
            data.layers.(i)
        )

        method set_values inputs = (
            let compute_layer 
                (layer      : float array) 
                (previous   : float array)
                (weights    : float array)
                (activation : ActivationFunction.func)
            = (
                let w = ref 0 in
                for i = 0 to Array.length layer - 1 do
                    let sum = ref 0. in
                    Array.iter (fun prev -> 
                        ignore (sum +.= (weights.(!w) *. prev));
                        ignore ((++) w)
                    ) previous;
                    layer.(i) <- activation !sum;
                done
            ) in
            Array.iteri (fun i input ->
                data.layers.(0).(i) <- input;
            ) inputs;
            for i = 1 to Array.length data.layers - 1 do 
                compute_layer
                    data.layers.(i) 
                    data.layers.(i - 1)
                    data.weights.(i - 1)
                    data.activations.(i - 1)
            done;
            data.layers.(Array.length data.layers - 1)
        )
        
        method set_training_method train =
            training <- train

        method train inputs outputs = (
            training inputs outputs (this :> network)
        )

    end

let new_network () =
    Random.self_init ();
    let layers = [| [|0.; 0.|]; [|0.; 0.|]; [|0.|] |] in
    let weights =
    [| 
        [|Random.float 1.; Random.float 1.; Random.float 1.; Random.float 1.|];
        [|Random.float 1.; Random.float 1.|]
    |] in
    let activations = [|ActivationFunction.linear; ActivationFunction.sigmoid|] in
    let data = {layers = layers; weights = weights; activations = activations} in
    new basic_network data;
