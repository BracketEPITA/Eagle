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

let contents = function
    | Some c -> c
    | None   -> raise (Invalid_argument "None has no contents")

let new_neuron () = {version = 0; value = 0.; parents = []; children = []}

class network epsilon (inputs : neuron array) (outputs : neuron array) (hidden : neuron array) =
    object (this)
        method sigma (x : float) =
            1. /. (1. +. exp(-. x))

        method set_values (l : float array) = 
            let sum = ref 0. in 
                Array.iteri (fun index v -> inputs.(index).value <- v) l;
                let recalculate neurons = 
                    Array.iter (fun n -> 
                        List.iter (fun p -> 
                            sum := !sum +. p.weight *. p.entry.value
                        ) n.parents;
                        n.value <- (this#sigma !sum);
                        sum := 0.;
                        n.version <- n.version + 1
                    ) neurons
                in 
                    recalculate hidden; 
                    recalculate outputs

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
            Array.iteri (fun i v ->
                this#outputsBackpropagation v output.(i)
            ) outputs
        )
    
    end

let new_network () = (
    Random.self_init();
    let nn i = new_neuron () in
    let inputs  = Array.init (28*28) nn in
    let outputs = Array.init (2*26 + 10 + 1) nn in
    let hidden  = Array.init 96 nn in
    let populate l1 l2 = (
        Array.iter (fun entry -> 
            Array.iter (fun exit -> 
                entry.children <- {weight = Random.float 1.; entry = entry; exit = exit}::entry.children
            ) l2
        ) l1;
    ) in
        populate inputs hidden;
        populate hidden outputs;

        new network 0.25 inputs outputs hidden;
)

