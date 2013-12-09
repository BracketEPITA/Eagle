let serialize path network = Network.(
    let out = open_out_bin path in
    let data = network#get_data in 
    Marshal.to_channel out data.size_inputs  [];
    Marshal.to_channel out data.size_outputs [];
    Marshal.to_channel out data.init_weights [];
    Marshal.to_channel out data.init_biases  [];
    close_out out;
)

let deserialize path = Network.(
    let input = open_in_bin path in
    let size_inputs  = (Marshal.from_channel input : int) in
    let size_outputs = (Marshal.from_channel input : int) in
    let init_weights = (Marshal.from_channel input : Matrix.matrix array) in
    let init_biases  = (Marshal.from_channel input : float array array) in
    close_in input;
    new basic_network {
        size_inputs  = size_inputs;
        size_outputs = size_outputs;
        init_weights = init_weights;
        init_biases  = init_biases;
    }
)
