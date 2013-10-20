open Math.Universe

type decomposition_state = {
    mutable lu : float Matrix.matrix;
    mutable piv : int array;
}

let array_swap i j arr =
    let v = arr.(i) in (
        arr.(i) <- arr.(j);
        arr.(j) <- v
    )

let init m =
    let lu = Matrix.clone m in
    let (w, h) = Matrix.dimension lu in
    let piv = Array.init w (fun i -> i) in
    for j = 0 to w-1 do 
        let lu_col = Array.init h (fun i -> lu.(i).(j)) in
        for i = 0 to h-1 do
            let kmax = min i j - 1 in
            let s = ref 0. in
            for k = 0 to kmax do
                ignore (s +.= lu.(i).(k) *. lu_col.(k));
            done;
            lu_col.(i) <- lu_col.(i) -. !s;
            lu.(i).(j) <- lu_col.(i);
        done;

        let p = ref j in
        for i = j + 1 to h-1 do 
            if abs_float lu_col.(i) > abs_float lu_col.(!p) then p := i
        done;
        if !p <> j then (
            (* make a line & col swap in matrix *)
            for k = 0 to w-1 do
                Matrix.swap (!p,k) (j,k) lu;
            done;
            array_swap !p j piv;
        );

        if j < h && lu.(j).(j) <> 0. then
            for i = j + 1 to h-1 do
                lu.(i).(j) <- lu.(i).(j) /. lu.(j).(j);
            done;
    done;
    {lu = lu; piv = piv}

let solve (inputs : float array) (state : decomposition_state) =
    let count = Array.length inputs in
    let b = Array.init count (fun i -> inputs.(state.piv.(i))) in
    let (dim,_) = Matrix.dimension state.lu in
    let x = Array.make count 0. in (
        for i = 0 to dim-1 do
            x.(i) <- b.(i);
            for j = 0 to i-1 do
                x.(i) <- x.(i) -. state.lu.(i).(j) *. x.(j);
            done;
        done;
        for i = dim-1 downto 0 do
            for j = dim-1 downto i+1 do
                x.(i) <- x.(i) -. state.lu.(i).(j) *. x.(j);
            done;
            x.(i) <- x.(i) /. state.lu.(i).(i);
        done;
        x
    )

