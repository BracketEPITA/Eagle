let serialize path network = Network.(
    let out = open_out_bin path in
    let data = network#get_data in 
    Marshal.to_channel out data.layers [];
    Marshal.to_channel out data.weights [];
    let activations = Array.init (Array.length data.activations) (fun i ->
        data.activations.(i).ActivationFunction.name
    ) in
    Marshal.to_channel out activations [];
)

let deserialize path = Network.(
    let input = open_in_bin path in
    let layers  = (Marshal.from_channel input : float array array) in
    let weights = (Marshal.from_channel input : float array array) in
    let raw_activations = (Marshal.from_channel input : string array) in
    let activations = Array.init (Array.length raw_activations) (fun i ->
        ActivationFunction.get raw_activations.(i)
    ) in
    new basic_network {
        layers  = layers;
        weights = weights;
        activations = activations;
    }
)
