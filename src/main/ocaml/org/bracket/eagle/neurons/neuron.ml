type neuron = {
    mutable version : int;
    mutable value : float;
    mutable parents : link list;
    mutable children : link list
} and link = {
    mutable weight : float;
    mutable entry : neuron;
    mutable exit : neuron
}

let new_neuron () = {version = 0; value = 0.; parents = []; children = []}

let fabs x = if x < 0. then x else -. x

class network epsilon (inputs : neuron array) (outputs : neuron array) (hidden : neuron array) =
    object (this)
        method sigma (x : float) =
            x /. (1. +. fabs(x))

        method set_values (l : float array) = 
            let sum = ref 0. in 
                Printf.printf "Input neurons :\n";
                Array.iteri (fun index v -> Printf.printf "n%d -> %f\n" index v; inputs.(index).value <- v) l;
                print_newline ();
                let recalculate neurons = 
                    Array.iteri (fun i n ->
                        List.iter (fun p ->
                            sum := !sum +. p.weight *. p.entry.value
                        ) n.parents;
                        Printf.printf "sum = %f\t" !sum;
                        n.value <- (this#sigma !sum);
                        Printf.printf "n%d -> %f\n" i n.value;
                        sum := 0.;
                        n.version <- n.version + 1;
                    ) neurons;
                    print_newline ();
                in 
                    Printf.printf "Hidden neurons :\n";
                    recalculate hidden;
                    Printf.printf "Output neurons :\n";
                    recalculate outputs

        method get_outputs =
            let l = Array.length outputs in
                Array.init l (fun i -> outputs.(i).value)

        method outputsBackpropagation (n : neuron) (expected : float) = 
            let delta = (n.value *. (1. -. n.value) *. (expected -. n.value)) *. epsilon *. n.value
            in let rec propagate = function
                | [] -> ()
                | h::t -> (
                        h.weight <- h.weight -. delta;
                        this#hiddenBackpropagation h.entry delta;
                        propagate t;
                    )
            in  propagate n.parents

        method hiddenBackpropagation (n : neuron) (delta : float) = 
            let rec outputsSum = function
                | [] -> 0.
                | h::t -> h.weight *. delta +. outputsSum t
            in let hiddenDelta = (n.value *. (1. -. n.value) *. outputsSum n.children) *. epsilon *. n.value
            in let rec propagate = function
                | [] -> ()
                | h::t -> (
                            h.weight <- h.weight -. hiddenDelta;
                            propagate t
                        )
            in propagate n.parents



        method train (input : float array) (output : float array) = (
            this#set_values(input);
            (*Array.iteri (fun i v ->
                this#outputsBackpropagation v output.(i)
            ) outputs*)
        )
    
    end

let new_network () = (
    Random.self_init();
    let nn i = new_neuron () in
    let inputs  = Array.init 2 nn in
    let outputs = Array.init 1 nn in
    let hidden  = Array.init 2 nn in
    let populate l1 l2 = (
        Array.iter (fun entry -> 
            Array.iter (fun exit -> 
                let w = Random.float 1. in
                    entry.children <- {weight = w; entry = entry; exit = exit}::entry.children;
                    exit.parents <- {weight = w; entry = entry; exit = exit}::exit.parents;
            ) l2
        ) l1;
    ) in
        populate inputs hidden;
        populate hidden outputs;

        new network 0.25 inputs outputs hidden;
)
