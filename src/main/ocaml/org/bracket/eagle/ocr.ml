let incr_char c = char_of_int (int_of_char c + 1)

let mkrange c1 c2 =
    let rec f l str =
        if l <= c2 then
            f (incr_char l) (str ^ String.make 1 l)
        else
            str
    in f c1 ""

let chars = (mkrange 'a' 'z') ^ (mkrange 'A' 'Z') ^ (mkrange '0' '9')

let getError learn_base pct = 
    
    let size = Array.length learn_base in
    let globalError = ref 0. in

    for i = 0 to size - 1 do
        let (i, o) = learn_base.(i) in
        let net = pct#feed i in
        let outputs = Array.length o in
        for j = 0 to outputs - 1 do
            let delta = o.(j) -. net.(j) in
            globalError := !globalError +. delta *. delta;
        done;
    done;
        !globalError /. float_of_int size



let init_network () = 
    let font = Sdlttf.open_font "Ubuntu-L.ttf" 7 in
    let data = FontUtils.generate_bitmap font chars in
    let network = ((((new NetworkFactory.factory)
        #add_layer Network.(input_matrix_size*input_matrix_size))
        #add_layer 50)
        #add_layer (10))
        #build
    in
    
    let error = ref 1. in
    let epoch = ref 0 in
    let epsilon = ref 0.3 in

    let lasterror = ref 1. in

    while !error > 0.1 do
        if !lasterror *. !error <= 0.0 then
            epsilon := min 0.9 (!epsilon +. 0.05)
        else
            epsilon := max (0.1) (!epsilon *. 0.99);
        incr epoch;
        network#train !epsilon data;
        error := getError data network;
        if !epoch mod 10 = 0 then
            Printf.printf "epoch %d : %.60f\n%!" !epoch !error;
        lasterror := !error;
    done;
    (network, data)


