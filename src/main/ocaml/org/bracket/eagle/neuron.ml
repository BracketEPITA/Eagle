type neuron = {
    mutable version  : int;
    mutable value    : float;
    mutable parents  : link list;
    mutable children : link list
} and link = {
    mutable weight   : float;
    mutable entry    : neuron;
    mutable exit     : neuron
}

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

class tokenizer fileName =

    object (this)

        val in_channel     = open_in fileName
        val delimiters     = ['['; ']'; ';'; ' '; '\t']

        val mutable line   = ""
        val mutable index  = 0
        val mutable length = 1
        val mutable token  = ""

        method next =
            try
                while token = "" do
                    print_string ("Next");
                    if index = length - 1 then
                        (
                            line   <- input_line in_channel;
                            index  <- 0;
                            length <- String.length line;
                        )
                    else ();

                    match (line.[index]) with
                        | '[' -> token <- "["
                        | ']' -> token <- "]"
                        | ':' -> token <- ":"
                        | _ -> (
                            let start = index in
                            index <- index + 1;
                            while index < length && not (this#isInList line.[index]) do
                                index <- index + 1
                            done;
                            token <- String.sub line start (index - start)
                        )
                done;
                token
            with End_of_file -> 
            (close_in in_channel;
            token <- "";
            token)

        method isInList x =
            let rec isInListRec x = function
                | []   -> false
                | h::t when h = x ->true
                | h::t -> isInListRec x t
            in isInListRec x delimiters

    end 

let deserialize fileName =
    let t      = new tokenizer fileName in 
    let token  = ref t#next in
    let i      = ref 0 in
    (*let neuron = ref new_neuron () in*)
    while ((!token) <> "" && !i < 10) do
        (
            print_string (!token ^ (string_of_int (!i)) ^ "\n");
            i := !i + 1;
            token := t#next;
        )

    done
