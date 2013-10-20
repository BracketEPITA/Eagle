
let main () = (
    let network = Neuron.new_network () in
    let inputs = [|1.; 0.|] and outputs = [|0.|] in
    for i = 0 to 40000 do
        network#train inputs outputs ;
    done;
    
    network#set_values inputs;
    Printf.printf "Success of %f\n" (Neuron.getvalue (network#getLayer 2).(0));

    exit 0
)

let _ = main ()

