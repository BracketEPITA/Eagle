let (++)  = Math.Universe.(++)
let (+.=) = Math.Universe.(+.=)
let (/.=) = Math.Universe.(/.=)

module ActivationFunction = struct
    type func = {
        f : (float -> float);
        derivative : (float -> float);
    }
    let linear = {
        f = (fun (x : float) -> x);
        derivative = (fun x -> 1.);
    }
    let sigmoid = {
        f = (fun x -> 1. /. (1. +. exp(-. x)));
        derivative = (fun x -> x *. (1. -. x) +. 0.1);
    }
    let tanh = {
        f = (fun x -> tanh x);
        derivative = (fun x -> 1. -. x *. x);
    }
    let sigmoid_alt = {
        f = (fun x -> x /. sqrt (1. +. x *. x));
        derivative = (fun x -> 
            let xs = x *. x in
            let xs2 = (xs +. 1.) *. (xs +. 1.) in 
                (1. -. xs) /. xs2
        )
    }
end

type dataset = {
    mutable inputs  : float array list;
    mutable outputs : float array list;
    mutable size    : int;
}

let new_dataset () =
    {inputs = []; outputs = []; size = 0}

let add_entry data inputs expected =
    data.inputs <- inputs::data.inputs;
    data.outputs <- expected::data.outputs

type network_data = {
    layers  : float array array;
    weights : float array array;
    biases  : float array array;
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
            fun 
                (pre : (int -> unit))
                (post : (int -> float -> unit))
                (data : dataset)
                (network : network) 
            -> () 
        )

        method get_layer i = (
            data.layers.(i)
        )

        method set_values inputs = (
            let compute_layer index = (
                let layer = data.layers.(index) in
                let previous = data.layers.(index - 1) in
                let weights = data.weights.(index - 1) in
                let activation = data.activations.(index - 1) in
                let w = ref 0 in
                let l = Array.length layer - 1 in
                for i = 0 to l do
                    let sum = ref 0. in
                    Array.iter (fun prev -> 
                        ignore (sum +.= (weights.(!w) *. prev));
                        ignore ((++) w)
                    ) previous;
                    layer.(i) <- activation.ActivationFunction.f !sum;
                done
            ) in
            Array.iteri (fun i input ->
                data.layers.(0).(i) <- input;
            ) inputs;
            for i = 1 to Array.length data.layers - 1 do 
                compute_layer i;
            done;
            data.layers.(Array.length data.layers - 1)
        )
        
        method set_training_method train =
            training <- train

        method train 
            ?pre:(pre=(fun epoch -> ())) 
            ?post:(post=(fun epoch error -> ())) 
            (data : dataset)
        = (
            training pre post data (this :> network)
        )

    end
