let (++)  = Math.Universe.(++)
let (+.=) = Math.Universe.(+.=)
let (/.=) = Math.Universe.(/.=)

let epsilon = 1.
let epoch   = ref 0
let error   = ref 1.
let threshold = 0.1

let rec propagate data (deltas, deltasBias) index (miniGrads, miniBias) (update: bool) prevGrads epsilons = 
    let outputs      = data.Network.layers.     (index)                 in
    let inputs       = data.Network.layers.     (index - 1)             in
    let successors   = data.Network.layers.     (index + 1)             in
    let weights      = data.Network.weights.    (index - 1)             in
    let biases       = data.Network.biases.     (index - 1)             in
    let succ_weights = data.Network.weights.    (index)                 in
    let succ_bias    = data.Network.biases.     (index)                 in
    let activation   = data.Network.activations.(index - 1)             in
    let diff         = activation.Network.ActivationFunction.derivative in
    
    let sum = ref 0. in
    Array.iteri (fun i e ->
        ignore (sum +.= (succ_weights.(i) *. deltas.(i)))
    ) successors;

    let sumBias = ref 0. in
    Array.iteri (fun i e ->
        ignore (sumBias +.= (succ_bias.(i) *. deltasBias.(i)))
    ) successors;

    
    let deltas      = Array.make (Array.length succ_weights) 0. in
    let deltasBias  = Array.make (Array.length succ_bias) 0.    in

    for i = 0 to Array.length outputs - 1 do

        let delta = diff outputs.(i) *. !sum in
        deltas.(i) <- delta;

        let deltaBias = diff outputs.(i) *. !sumBias in
        deltasBias.(i) <- deltaBias;

        for j = 0 to Array.length inputs - 1 do
            miniGrads.(index - 1).(j) <- miniGrads.(index - 1).(j) +. delta     *. inputs.(j);
            miniBias. (index - 1).(j) <- miniBias. (index - 1).(j) +. deltaBias *. inputs.(j);
            if update then 
                (
                    if miniGrads.(index - 1).(j) *. prevGrads.(index - 1).(j) >= 0. then
                        epsilons.(index - 1).(j) <- max (epsilons.(index - 1).(j) +. 1.) 100.
                    else
                        epsilons.(index - 1).(j) <- min (epsilons.(index - 1).(j) *. 0.75) 0.000001;

                    if !error < 10. *. threshold then
                        epsilons.(index - 1).(j) <- 1.;
                    weights.(j) <- weights.(j) -. miniGrads.(index - 1).(j) *. epsilons.(index - 1).(j);
                    biases. (j) <- biases. (j) -. miniBias. (index - 1).(j) *. epsilons.(index - 1).(j);

                )
        done;
    done;
    if index > 1 then propagate data (deltas, deltasBias) (index - 1) (miniGrads, miniBias) update prevGrads epsilons

let backpropagation data index expected (miniGrads, miniBias) (update: bool) prevGrads epsilons =
    let outputs     = data.Network.layers.     (index)                 in
    let inputs      = data.Network.layers.     (index - 1)             in
    let weights     = data.Network.weights.    (index - 1)             in
    let activation  = data.Network.activations.(index - 1)             in
    let diff        = activation.Network.ActivationFunction.derivative in


    let deltas      = Array.make (Array.length outputs) 0.  in
    let deltasBias  = Array.make (Array.length outputs) 0.  in
    for i = 0 to Array.length outputs - 1 do
        let delta = diff outputs.(i) *. (outputs.(i) -. expected.(i)) in
        deltas.(i) <- delta;
        
        for j = 0
         to Array.length inputs - 1 do
            miniGrads.(index - 1).(j)   <- miniGrads.(index - 1).(j) +. delta *. inputs.(j);
            if update then 
                (
                    if miniGrads.(index - 1).(j) *. prevGrads.(index - 1).(j) >= 0. then
                        epsilons.(index - 1).(j) <- max (epsilons.(index - 1).(j) +. 1.) 100.
                    else
                        epsilons.(index - 1).(j) <- min (epsilons.(index - 1).(j) *. 0.75) 0.000001;
                    if !error < 10. *. threshold then
                        epsilons.(index - 1).(j) <- 1.;
                    weights.(j)     <- weights.(j) -. miniGrads.(index - 1).(j) *. epsilons.(index - 1).(j);

                )
        done;
    done;

    propagate data (deltas, deltasBias) (index - 1) (miniGrads, miniBias) update prevGrads epsilons


let train_entry (inputs, outputs) network (miniGrads, miniBias) (update : bool) prevGrads epsilons =
    ignore(network#set_values inputs);
    let data        = network#get_data                                  in
    let index       = Array.length data.Network.layers - 1              in

    backpropagation data index outputs (miniGrads, miniBias) update prevGrads epsilons

let rec train_batch batch network (miniGrads, miniBias) prevGrads epsilons =
    if Array.length batch = 1 then 
        train_entry batch.(0) network (miniGrads, miniBias) true prevGrads epsilons
    else
        if Array.length batch != 0 then
            for i = 0 to (Array.length batch - 1) do
                train_entry batch.(i) network (miniGrads, miniBias) false prevGrads epsilons
            done;
            train_entry batch.(Array.length batch - 1) network (miniGrads, miniBias) true prevGrads epsilons

let getError (inputs, outputs) network = 
    ignore(network#set_values inputs);
    
    let global_error = ref 0.                               in
    let data         = network#get_data                     in
    let i            = Array.length data.Network.layers - 1 in
    
    Array.iteri (fun j output ->
        let delta = output -. data.Network.layers.(i).(j) in
        ignore (global_error +.= (delta *. delta))
    ) outputs;
    !global_error /. float_of_int (Array.length data.Network.layers.(i))

let length (l1, l2) =
    let rec lengthRec i = function
        | ([], []) -> i
        | (e1::l1, e2::l2) -> lengthRec (i + 1) (l1, l2)
        | _ -> failwith "inputs and outputs are not in pairs."
    in lengthRec 0 (l1, l2)


 
let train pre post (data : Network.dataset) network =
        epoch   := 0 ;
        error   := 1.;

        let inputs  = data.Network.inputs   in
        let outputs = data.Network.outputs  in
        
        let datas   = network#get_data      in
        let dataset = Array.make (length (inputs, outputs)) ([|0.|], [|0.|]) in
        
        let rec makeDataSet i = function 
            | ([], []) -> ()
            | (e1::l1, e2::l2) -> (
                    dataset.(i) <- (e1, e2);
                    makeDataSet (i + 1) (l1, l2)
                )
            | _ -> failwith "inputs and outputs are not in pairs."
        in makeDataSet 0 (inputs, outputs);

        let epsilons = Array.init (Array.length datas.Network.weights) (fun i -> 
            Array.make (Array.length datas.Network.weights.(i)) 1.
        ) in

        let miniGrads = Array.init (Array.length datas.Network.weights) (fun i -> 
            Array.make (Array.length datas.Network.weights.(i)) 0.
        ) in
        let miniBias = Array.init (Array.length datas.Network.biases) (fun i -> 
            Array.make (Array.length datas.Network.biases.(i)) 0.
        ) in
        let prevGrads = Array.init (Array.length datas.Network.weights) (fun i -> 
            Array.make (Array.length datas.Network.weights.(i)) 0.
        ) in
        Random.self_init ();


        while !error > threshold do
        
            (* Init *)
            error := 0.;

            for i = 0 to (Array.length datas.Network.weights - 1) do
                for j = 0 to (Array.length miniGrads.(i) - 1) do
                    prevGrads.(i).(j) <- miniGrads.(i).(j);
                    miniGrads.(i).(j) <- 0.;
                done;
            done;

            for i = 0 to (Array.length miniBias - 1) do
                for j = 0 to (Array.length miniBias.(i) - 1) do
                    miniBias.(i).(j) <- 0.;
                done;
            done;
            
            

            (* Shuffling the dataset using Fisher–Yates shuffle *)
            for i = (Array.length dataset - 1) downto 0 do
                let j = Random.int (i + 1) in 
                (* Just a swap *)
                let temp = dataset.(i) in
                dataset.(i) <- dataset.(j);
                dataset.(j) <- temp;
            done;

            pre !epoch;

            let split = 2 in
            let size = (Array.length dataset / split) in
            let sets = Array.init split (fun i ->
                let l = i * size in
                let u = min (l + size) (Array.length dataset - l) in
                Array.sub dataset l u
            ) in
            Array.iter (fun dataset ->
                train_batch dataset network (miniGrads, miniBias) prevGrads epsilons;
            ) sets;

            for i = 0 to (Array.length dataset - 1) do
                ignore (error  +.= getError dataset.(i) network);
            done;
            
            ignore (error /.= (float) (Array.length dataset));
            post !epoch !error;
            ignore ((++) epoch);
        done;

