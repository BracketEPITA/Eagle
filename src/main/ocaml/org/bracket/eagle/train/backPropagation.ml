let (++)  = Math.Universe.(++)
let (+.=) = Math.Universe.(+.=)
let (/.=) = Math.Universe.(/.=)

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
            ignore ((++) w)
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

let train_entry inputs outputs network =
    ignore (network#set_values inputs);
    let data = network#get_data in
    let i = Array.length data.Network.layers - 1 in
    let global_error = ref 0. in
    propagate_outputs data i outputs;
    Array.iteri (fun j output ->
        let delta = output -. data.Network.layers.(i).(j) in
        ignore (global_error +.= (delta *. delta))
    ) outputs;
    !global_error /. float_of_int (Array.length data.Network.layers.(i))

let train pre post (data : Network.dataset) network =
    try
        let epoch = ref 0 in
        let error = ref 1. in
        while !error > 0.0001 do
            pre !epoch;
            error := 0.;
            let length = ref 0. in
            let rec f input output = (
                match (input, output) with
                    | (e1::l1, e2::l2) -> (
                        f l1 l2;
                        ignore (error +.= train_entry e1 e2 network);
                        ignore (length +.= 1.);
                    )
                    | ([], [])         -> ()
                    | _ -> failwith "inputs and outputs are not in pairs."
            ) in 
            f data.Network.inputs data.Network.outputs;
            ignore (error /.= !length);
            post !epoch !error;
            ignore ((++) epoch);
        done;
    with _ -> failwith "Failed to train network using the dataset."
