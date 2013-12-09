let make_out i n =
    let arr = Array.make n 0. in
    arr.(i) <- 1.0; arr

let to_binary c =
    let v = int_of_char c in
    let rec f i l =
        if i <> 0 then
            f (i lsr 1) ((float_of_int (i mod 2))::l)
        else l
    in
    let arr = Array.make 10 0.0 in
    let res = Array.of_list (f v []) in
    let shift = (Array.length arr) - (Array.length res) in
    Array.iteri (fun i e ->
       arr.(i + shift) <- e 
    ) res;
    arr

let from_binary l =
    let v = ref 0 in
    Array.iter (fun e ->
        v := (!v lsl 1) + (if e > 0.5 then 1 else 0)
    ) l;
    char_of_int !v

let generate_bitmap font str = 
    let glyphnum = String.length str in 
    let dataset = Array.make glyphnum ([||], [||]) in
    for i = 0 to glyphnum - 1 do
        let char = String.unsafe_get str i in
        let size = Network.input_matrix_size in
        let glyph = Sdlttf.(render_glyph font (SOLID Sdlvideo.black) char) in
        let get   = Sdlvideo.get_pixel_color glyph in
        let (w,h) = SDLUtils.get_dims glyph in
        let input = Array.make (size * size) (-1.0) in
        SDLUtils.imageiter (fun i j ->
            input.(i + j*size) <- 
                if SDLUtils.level (get i j) = 255.0 then 1.0 else -1.0; 
        ) glyph;
        dataset.(i) <- (input, (to_binary char));
    done;
    dataset
