type neuron = {
    mutable version : int;
    mutable value : float;
    mutable parents : link list;
    mutable childrens : link list
} and link = {
    mutable weight : float;
    mutable input : neuron;
    mutable output : neuron
}

let contents = function
    | Some c -> c
    | None   -> raise (Invalid_argument "None has no contents")

class network =
    object (this)
        val mutable inputs  = (Array.make (28 * 28) None : neuron option array)
        val mutable outputs = (Array.make (2*26 + 10) None : neuron option array)
        val mutable hidden  = (Array.make 96 None : neuron option array) (* 96 neurons *)
        
        method sigma (x : float) =
            1. /. (1. +. exp(-. x))

        method set_input i neuron =
            inputs.(i) <- neuron
        
        method set_output i neuron =
            outputs.(i) <- neuron
        
        method set_values (l : float array) = let sum = ref 0. in (
            Array.iteri (fun index v -> if inputs.(index) <> None then (contents inputs.(index)).value <- v) l;
            let recalculate (neurons : neuron option array) = 
                Array.iter (fun val1 -> 
                    if (val1 <> None) then (
                        List.iter (fun val2 -> 
                            sum := !sum +. val2.weight *. val2.input.value
                        ) (contents val1).parents;
                        (contents val1).value <- (this#sigma !sum);
                        sum := 0.;
                        (contents val1).version <- (contents val1).version + 1
                   )
                ) neurons in
            (recalculate hidden; recalculate outputs)
        )

    method backwardPropagation (n : neuron) (expected : float) (result : float) = 
        let rec propagate = function
        | [] -> ()
        | h::t -> (
                    h.weight <- h.weight +. (expected -. result) *. (expected -. result) *. 0.25 *. n.value;
                    propagate t;
                )
        in  propagate n.parents
    
    method train (input : float array) (output : float array) = 
        this#set_values(input);
        for i = 0 to Array.length outputs do
            this#backwardPropagation (contents outputs.(i)) (contents outputs.(i)).value input.(i)
        done

    end
