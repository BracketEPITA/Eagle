let iteri f l = 
    let rec iteri_rec i = function
        | []   -> ()
        | h::t -> (f i h; iteri_rec (i + 1) t)
    in iteri_rec 0 l

type neuron = {
    mutable version  : int;
    mutable value    : float;
    mutable parents  : link list;
    mutable children : link list;
    mutable index    : int;
} and link = {
    mutable weight   : float;
    mutable entry    : neuron;
    mutable exit     : neuron
}

let new_neuron () = {version = 0; value = 0.; parents = []; children = []; index = 0}

let fabs x = if x < 0. then x else -. x

class network epsilon (inputs : neuron array) (outputs : neuron array) (hidden : neuron array) =
    object (this)
        method sigma (x : float) =
            x /. (1. +. fabs(x))
        method getLayer = function
            | 0 -> inputs
            | 1 -> hidden
            | 2 -> outputs
            | _ -> inputs

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


let serealize net file = 
    let oc = open_out file in
    for i = 0 to 3 do
        let layer = net#getLayer i in
        for j = 0 to Array.length layer do
            let inputs = ref "" in
            iteri (
                fun index value -> (
                    inputs := !inputs ^ (Printf.sprintf "l%d:w%f:n%d" index
                    value.weight value.exit.index)
                )
            ) (layer.(j)).parents; 
            let outputs = ref "" in
            iteri (
                fun index value -> (
                    outputs := !inputs ^ (Printf.sprintf "l%d:w%f:n%d" index
                        value.weight value.exit.index )
                )
            ) (layer.(j)).children;
            Printf.fprintf oc "n%d %d [%s] [%s]\n" j i !inputs !outputs 

        done
    done

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
