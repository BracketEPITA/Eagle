open Math.Universe

let epsilon = 0.79

let rec propagate data index deltas = 
    let outputs      = data.Network.layers.(index) in
    let inputs       = data.Network.layers.(index - 1) in
    let successors   = data.Network.layers.(index + 1) in
    let weights      = data.Network.weights.(index - 1) in
    let succ_weights = data.Network.weights.(index) in
    let w = ref 0 in
    let sum = ref 0. in
    Array.iteri (fun i succ ->
        ignore (sum +.= (succ_weights.(!w) *. deltas.(!w)));
        ignore ((++) w)
    ) successors;
    w := 0;
    let delta_index = ref 0 in
    let deltas = Array.make (Array.length succ_weights) 0. in
    Array.iteri (fun i output ->
        let delta = output *. (1. -. output) *. !sum in
        deltas.(!delta_index) <- delta;
        ignore ((++) delta_index);
        Array.iteri (fun j input ->
            weights.(!w) <- weights.(!w) -. delta *. inputs.(j) *. epsilon;
            ignore ((++) w)
        ) inputs
    ) outputs;
    if index > 1 then propagate data (index - 1) deltas

let propagate_outputs data index expected =
    let outputs = data.Network.layers.(index) in
    let inputs  = data.Network.layers.(index - 1) in
    let weights = data.Network.weights.(index - 1) in
    let w = ref 0 in
    let delta_index = ref 0 in
    let deltas = Array.make (Array.length weights) 0. in
    Array.iteri (fun i output ->
        let delta = output *. (1. -. output) *. (output -. expected.(i)) in
        deltas.(!delta_index) <- delta;
        ignore ((++) delta_index);
        Array.iteri (fun j input ->
            weights.(!w) <- weights.(!w) -. delta *. inputs.(j) *. epsilon;
            ignore ((++) w)
        ) inputs
    ) outputs;
    propagate data (index - 1) deltas

    
let train inputs outputs network =
    ignore (network#set_values inputs);
    let data = network#get_data in
    let i = Array.length data.Network.layers - 1 in
    propagate_outputs data i outputs;
    let global_error = ref 0. in
    Array.iteri (fun j output ->
        let delta = output -. data.Network.layers.(i).(j) in
        ignore (global_error +.= (delta *. delta))
    ) outputs;
    !global_error /. float_of_int (Array.length data.Network.layers.(i))
