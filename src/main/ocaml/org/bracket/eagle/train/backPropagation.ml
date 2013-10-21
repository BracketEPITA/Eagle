open Math.Universe

let epsilon = 1.

let rec propagate data index deltas = 
    let outputs      = data.Network.layers.(index) in
    let inputs       = data.Network.layers.(index - 1) in
    let successors   = data.Network.layers.(index + 1) in
    let weights      = data.Network.weights.(index - 1) in
    let succ_weights = data.Network.weights.(index) in
    let activation   = data.Network.activations.(index - 1) in
    let diff         = activation.Network.ActivationFunction.derivative in
    let w   = ref 0 in
    let sum = ref 0. in
    Array.iter (fun i ->
        ignore (sum +.= (succ_weights.(!w) *. deltas.(!w)));
        ignore ((++) w)
    ) successors;
    w := 0;
    let delta_index = ref 0 in
    let deltas = Array.make (Array.length succ_weights) 0. in
    for i = 0 to Array.length outputs - 2 do
        let output = outputs.(i) in
        let delta = diff output *. !sum in
        deltas.(!delta_index) <- delta;
        ignore ((++) delta_index);
        for j = 0 to Array.length inputs - 2 do
            weights.(!w) <- weights.(!w) -. delta *. inputs.(j) *. epsilon;
            ignore (Math.Universe.(++) w)
        done;
    done;
    if index > 1 then propagate data (index - 1) deltas

let propagate_outputs data index expected =
    let outputs = data.Network.layers.(index) in
    let inputs  = data.Network.layers.(index - 1) in
    let weights = data.Network.weights.(index - 1) in
    let activation = data.Network.activations.(index - 1) in
    let diff = activation.Network.ActivationFunction.derivative in
    let w = ref 0 in
    let delta_index = ref 0 in
    let deltas = Array.make (Array.length weights) 0. in
    for i = 0 to Array.length outputs - 1 do
        let output = outputs.(i) in
        let delta = diff output *. (output -. expected.(i)) in
        deltas.(!delta_index) <- delta;
        ignore ((++) delta_index);
        for j = 0 to Array.length inputs - 2 do
            weights.(!w) <- weights.(!w) -. delta *. inputs.(j) *. epsilon;
            ignore ((++) w)
        done;
    done;
    propagate data (index - 1) deltas

let train inputs outputs network =
    let o = network#set_values inputs in
    let data = network#get_data in
    let i = Array.length data.Network.layers - 1 in
    propagate_outputs data i outputs;
    let global_error = ref 0. in
    Array.iteri (fun j output ->
        let delta = output -. data.Network.layers.(i).(j) in
        ignore (global_error +.= (delta *. delta))
    ) outputs;
    !global_error /. float_of_int (Array.length data.Network.layers.(i))

