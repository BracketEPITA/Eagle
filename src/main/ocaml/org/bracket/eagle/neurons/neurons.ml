type neuron = {
    mutable version : int;
    mutable value : int;
    mutable inputs : link list;
    mutable outputs : link list
} and link = {
    mutable weight : int;
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
        
        method set_values (l : int array) = let sum = ref 0 in (
            Array.iteri (fun e i -> if inputs.(i) <> None then (contents inputs.(i)).value <- e) l;
            let recalculate (neurons : neuron option array) = 
                Array.iter (fun e1 -> 
                    if (e1 <> None) then (
                        List.iter (fun e2 -> 
                            sum := !sum + e2.weight * e2.input.value
                        ) (contents e1).inputs;
                        (contents e1).value <- (int_of_float (this#sigma (float_of_int !sum)));
                        sum := 0;
                        (contents e1).version <- (contents e1).version + 1
                   )
                ) neurons in
            (recalculate hidden; recalculate outputs)
        )
    end

