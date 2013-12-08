let incr_char c = char_of_int (int_of_char c + 1)

let mkrange c1 c2 =
    let rec f l str =
        if l <= c2 then
            f (incr_char l) (str ^ String.make 1 l)
        else
            str
    in f c1 ""

let chars = (*(mkrange 'a' 'z') ^ (mkrange 'A' 'Z') ^ *)(mkrange '0' '9')

let init_network () =
    let arial = Sdlttf.open_font "Arial.ttf" 7 in
    let data = FontUtils.generate_bitmap arial chars in
    let network = (((((new NetworkFactory.factory)
        #add_layer {
            NetworkFactory.size = Network.(input_size * input_size); 
            NetworkFactory.has_bias = true; 
            NetworkFactory.activation = Network.ActivationFunction.sigmoid
        })
        #add_layer {
            NetworkFactory.size = 100;
            NetworkFactory.has_bias = true;
            NetworkFactory.activation = Network.ActivationFunction.sigmoid
        })
        #add_layer {
            NetworkFactory.size = 100;
            NetworkFactory.has_bias = true;
            NetworkFactory.activation = Network.ActivationFunction.sigmoid
        })
        #add_layer {
            NetworkFactory.size = String.length chars;
            NetworkFactory.has_bias = false;
            NetworkFactory.activation = Network.ActivationFunction.sigmoid
        })
        #build
    in
    network#set_training_method (BackPropagation.train);
    network#train ~post:(fun i error ->
        if i mod 100 = 0 then 
            Printf.printf "epoch %d : error = %.64f\n%!" i error;
    ) data;
    (network, data)

let get_char network matrix = (
    let result = network#set_values matrix in
    Array.iteri (fun i e ->
        Printf.printf "%d -> %.30f\n" i e;
    ) result;
    FontUtils.from_binary result;
)
