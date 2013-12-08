let make_output i n =
    let arr = Array.make n 0.0 in
    arr.(i) <- 1.0; arr

let to_binary c =
    Printf.printf "%c = %d\n" c (int_of_char c);
    let v = int_of_char c in
    let rec f i l =
        if i <> 0 then
            f (i lsr 1) ((float_of_int (i mod 2))::l)
        else l
    in
    let arr = Array.make 10 0.0 in
    let res = Array.of_list (f v []) in
    Array.iteri (fun i e ->
       arr.(i) <- e 
    ) res;
    arr

let from_binary l =
    let v = ref 0 in
    Array.iter (fun e ->
        v := (!v lsl 1) + (if e > 0.5 then 1 else 0)
    ) l;
    char_of_int !v

let generate_bitmap font str = 
    let dataset = Network.new_dataset () in
    let glyphnum = String.length str in 
    for i = 0 to glyphnum - 1 do
        let char = String.unsafe_get str i in
        let size = Network.input_size in
        let glyph = Sdlttf.(render_glyph font (SOLID Sdlvideo.black) char) in
        let get   = Sdlvideo.get_pixel_color glyph in
        let (w,h) = SDLUtils.get_dims glyph in
        let input = Array.make (w * h) (-1.0) in
        SDLUtils.imageiter (fun i j ->
            input.(i + j*size) <- 
                if SDLUtils.level (get i j) = 255.0 then 1.0 else -1.0; 
        ) glyph;
        let bin = to_binary char in
        Array.iter (fun e -> 
            Printf.printf "%d" (int_of_float e);
        );
        print_newline ();
        Network.add_entry dataset input (to_binary char);
        Array.iteri (fun i e ->
            if i mod w = 0 then print_newline ();
            Printf.printf "%f " e;
        ) input;
    done;
    dataset
