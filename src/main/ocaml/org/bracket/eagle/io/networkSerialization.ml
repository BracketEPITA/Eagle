let serialize path network = Network.(
    let out = open_out_bin path in
    let data = network#get_data in 
    Marshal.in_channel out data.layers;
    Marshal.in_channel out data.weights;
    let activations = Array.init (Array.length data.activations) (fun i ->
        data.activations.(i).name
    ) in
    Marshal.in_channel out activations;
)
