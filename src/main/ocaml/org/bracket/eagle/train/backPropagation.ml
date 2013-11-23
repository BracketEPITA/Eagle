let (++)  = Math.Universe.(++)
let (+.=) = Math.Universe.(+.=)
let (/.=) = Math.Universe.(/.=)

let epsilon = 1.

let rec propagate data deltas index = 
    let outputs      = data.Network.layers.     (index)                 in
    let inputs       = data.Network.layers.     (index - 1)             in
    let successors   = data.Network.layers.     (index + 1)             in
    let weights      = data.Network.weights.    (index - 1)             in
    let succ_weights = data.Network.weights.    (index)                 in
    let activation   = data.Network.activations.(index - 1)             in
    let diff         = activation.Network.ActivationFunction.derivative in
    
    let sum = ref 0. in
    Array.iteri (fun i e ->
        ignore (sum +.= (succ_weights.(i) *. deltas.(i)))
    ) successors;
    
    let deltas = Array.make (Array.length succ_weights) 0. in
    
    for i = 0 to Array.length outputs - 2 do
        let output = outputs.(i) in
        let delta = diff output *. !sum in
        deltas.(i) <- delta;

        for j = 0 to Array.length inputs - 2 do
            weights.(j) <- weights.(j) -. delta *. inputs.(j) *. epsilon;
        done;
    done;
    if index > 1 then propagate data deltas (index - 1)

let backpropagation data deltas index =
    let outputs     = data.Network.layers.      (index)                 in
    let inputs      = data.Network.layers.      (index - 1)             in
    let weights     = data.Network.weights.     (index - 1)             in

    for i = 0 to Array.length outputs - 1 do
        for j = 0 to Array.length inputs - 2 do
            weights.(j) <- weights.(j) -. deltas.(i) *. inputs.(j) *. epsilon
        done 
    done;
    propagate data deltas (index - 1)


let setErrors deltas inputs expected network =
    ignore(network#set_values inputs);
    let data        = network#get_data                                  in
    let index       = Array.length data.Network.layers - 1              in 

    let outputs     = data.Network.layers.      (index)                 in
    let activation  = data.Network.activations. (index - 1)             in
    let diff        = activation.Network.ActivationFunction.derivative  in
    let globalError = ref 0.                                            in
    
    for i = 0 to Array.length outputs - 1 do
        let out = outputs.(i) in
        deltas.(i) <- (deltas.(i) +. diff out *. (out -. expected.(i))) /. 2.;
        globalError := !globalError +. deltas.(i);
    done;
    !globalError


let train pre post (data : Network.dataset) network =
    try
        let epoch = ref 0                                                    in
        let error = ref 1.                                                   in
        
        let networkData = network#get_data.Network.layers                    in
        let size = Array.length (networkData.(Array.length networkData - 1)) in
        let deltas = Array.make size 0. in

        while !error > 0.0001 do
            pre !epoch;
            error := 0.;
            let length = ref 0. in
            let rec f input output = (
                match (input, output) with
                    | (e1::l1, e2::l2) -> (
                        f l1 l2;
                        ignore (error  +.= setErrors deltas e1 e2 network);
                        ignore (length +.= 1.);
                    )
                    | ([], [])         -> ()
                    | _ -> failwith "inputs and outputs are not in pairs."
            ) in 
            f data.Network.inputs data.Network.outputs;
            backpropagation network#get_data deltas size;
            ignore (error /.= !length);
            post !epoch !error;
            ignore ((++) epoch);
        done;
    with _ -> failwith "Failed to train network using the dataset."

