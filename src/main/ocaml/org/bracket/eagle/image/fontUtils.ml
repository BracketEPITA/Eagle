let make_output i n =
    let arr = Array.make n 0.0 in
    arr.(i) <- 1.0; arr

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
        let k = ref 0 in
        SDLUtils.imageiter (fun i j ->
            input.(!k) <- 
                if SDLUtils.level (get i j) = 255.0 then 1.0 else -1.0;
            incr k;
        ) glyph;
        Network.add_entry dataset input (make_output i glyphnum);
        Array.iteri (fun i e ->
            if i mod w = 0 then print_newline ();
            Printf.printf "%f " e;
        ) input;
    done;
    dataset
