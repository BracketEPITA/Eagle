type 'a matrix = 'a array array

let make (w,h) v = Array.make_matrix w h v
let init0 (w,h) f = 
    let m = make (w,h) 0 in
    for i = 0 to w-1 do
        for j = 0 to h-1 do
            m.(i).(j) <- f (i,j)
        done
    done; m

let init (w,h) f = Array.init h (fun j -> Array.init w (fun i -> f (i,j)))
let init2 d f = init (d,d) f
let identity n = init (n,n) (fun (i,j) -> if i = j then 1 else 0)
let dimension (m : 'a matrix) = ((Array.length m.(0)), (Array.length m))

let clone (m : 'a matrix) = 
    let (w,h) = dimension m in init (w,h) (fun (i,j) -> m.(j).(i))

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

let mul0 (m1 : 'a matrix) (m2 : 'a matrix) prod add zero = 
    let (w1,h1) = dimension m1 in
    let (w2,h2) = dimension m2 in
    if h1 <> w2 then raise (Invalid_argument "Matrices are not compatible.")
    else init (w1,h2) (fun (i,j) -> 
        let rec f k = 
            if k >= h1 then zero
            else add (prod m1.(i).(k) m2.(k).(j)) (f (k + 1))
        in f 0
    )

let mul m1 m2 = mul0 m1 m2 ( * ) ( + ) 0
let mulf m1 m2 = mul0 m1 m2 ( *. ) ( +. ) 0.

let print_matrix printf (m : 'a matrix) =
    iterij (fun (i,j) e -> 
        if i = 0 then print_newline ();
        printf e;
        print_string "\t";
    ) m

