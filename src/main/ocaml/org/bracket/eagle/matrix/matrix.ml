type 'a matrix = 'a array array

let make (w,h) v =
    Array.make h (Array.make w v)

let init (w,h) f =
    Array.init h (fun j ->
        Array.init w (fun i -> 
            f (i,j)
        )
    )

let dimension (m : 'a matrix) = ((Array.length m.(0)), (Array.length m))

let clone (m : 'a matrix) = let (w,h) = dimension m in init (w,h) (fun (i,j) -> m.(j).(i))

let iter f (m : 'a matrix) =
    Array.iter (fun row ->
        Array.iter (fun e -> 
            f e
        ) row
    ) m


let iterij f (m : 'a matrix) =
    Array.iteri (fun j row ->
        Array.iteri (fun i e -> 
            f (i,j) e
        ) row
    ) m

let swap (i,j) (k,l) (m : 'a matrix) =
    let swp = m.(i).(j) in (
        m.(i).(j) <- m.(k).(l);
        m.(k).(l) <- swp
    )

let print_matrix printf (m : 'a matrix) =
    iterij (fun (i,j) e -> 
        if i = 0 then print_newline ();
        printf e;
        print_string "\t";
    ) m

