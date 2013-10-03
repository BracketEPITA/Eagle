class neuron =
    object (this)
        val mutable version = 0
        val mutable value = 0
        val mutable inputs  = ([] : link list)
        val mutable outputs = ([] : link list)
        method sigma x = 
            1. /. (1. +. exp(-x))
        method back_propagation =
            ()
    end
and link =
    object (this)
        val mutable weight = 0
        val mutable input  = (None : neuron option)
        val mutable output = (None : neuron option)
        method get_input = 
            input
        method get_output = 
            output
        method set_input value =
            input <- value
        method set_output value =
            output <- value
        method get_weight = 
            weight
        method set_weight value =
            weight <- value
    end;;

class network =
    object (this)
        val mutable inputs  = (Array.make (28 * 28) None : neuron array)
        val mutable outputs = (Array.make (2*26 + 10) None : neuron array)
        val mutable hidden  = (Array.make 96 None : neuron array) (* 96 neurons *)
        method add_input neuron =
            inputs <- inputs::neuron
        method add_output neuron =
            outputs <- outputs::neuron
        method set_values (l : int array) = let sum = ref 0 in (
            for i = 0 to (Array.length inputs) do (
                Array.set inputs i (Array.get i l)
            ) done;
            for i = 0 to (Array.length hidden) do (
                let rec set_hidden v = 
                    match v#get_inputs with
                        | e::l -> 
                            (sum := sum + e#get_weight * e#get_input#get_value)
                        |   [] -> ()
                in set_hidden (Array.get hidden i)
            ) done;
        )
    end;;

