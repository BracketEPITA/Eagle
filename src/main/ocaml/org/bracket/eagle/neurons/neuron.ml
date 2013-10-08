type neuron = {
    mutable version : int;
    mutable value : float;
    mutable parents : link list;
    mutable childrens : link list
} and link = {
    mutable weight : float;
    mutable entry : neuron;
    mutable exit : neuron
}

let contents = function
    | Some c -> c
    | None   -> raise (Invalid_argument "None has no contents")

class network =
    object (this)
        val mutable inputs  = (Array.make (28 * 28) None : neuron option array)
        val mutable outputs = (Array.make (2*26 + 10) None : neuron option array)
        val mutable hidden  = (Array.make 96 None : neuron option array) (* 96 neurons *)
        val epsilon = 0.25
        
        method sigma (x : float) =
            1. /. (1. +. exp(-. x))

        method set_input i neuron =
            inputs.(i) <- neuron
        
        method set_output i neuron =
            outputs.(i) <- neuron
        
        method set_values (l : float array) = let sum = ref 0. in (
            Array.iteri (fun index v -> if inputs.(index) <> None then (contents inputs.(index)).value <- v) l;
            let recalculate (neurons : neuron option array) = 
                Array.iter (fun n -> 
                    if (n <> None) then (
                        List.iter (fun p -> 
                            sum := !sum +. p.weight *. p.entry.value
                        ) (contents n).parents;
                        (contents n).value <- (this#sigma !sum);
                        sum := 0.;
                        (contents n).version <- (contents n).version + 1
                   )
                ) neurons in
            (recalculate hidden; recalculate outputs)
        )
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
            in let hiddenDelta = (n.value *. (1. -. n.value) *. outputsSum n.childrens) *. epsilon *. n.value
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
                this#outputsBackpropagation (contents v) output.(i)
            ) outputs
        )
    
    end

