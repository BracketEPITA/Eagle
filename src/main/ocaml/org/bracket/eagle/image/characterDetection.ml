let findAdjacent (x, y) (src, dst) imgArray (yMini, yMaxi) (x0, y0, xMax,
yMax) = 
    let (localXMin, localXMax, localYMin, localYMax) = (ref x, ref x, ref y, ref y) in
    let rec findAdjacentRec x y =
        if x > x0 && x < xMax && y > y0 && y < yMax then
            if Sdlvideo.get_pixel_color src x y = Sdlvideo.black then
                if not(imgArray.(y).(x)) then 
                    (
                        imgArray.(y).(x) <- true;
    
                        localXMin := min !localXMin x;
                        localXMax := max !localXMax x;

                        localYMin := min !localYMin y;
                        localYMax := max !localYMax y;

                        let average20 = (yMaxi - yMini) / 2  in
                        for y = max (y - average20) yMini 
                        to min (y + average20) yMaxi do
                            for x = x - 2 to x + 2 do
                                findAdjacentRec x y
                            done
                        done
                    )
    in 
        findAdjacentRec x y;
        (!localXMin, !localXMax, !localYMin, !localYMax)


let put_pixel = Sdlvideo.put_pixel_color

let mini lines y y0 =
    let r = ref y in
    while !r >= y0 && (not lines.(!r)) do
        r := !r - 1;
    done;
    !r

let maxi lines y h =
    let r = ref y in
    while !r < h && (not lines.(!r)) do
        r := !r + 1;
    done;
    !r

let getMinMax lines y (y0, yMax) = (mini lines y y0, maxi lines y yMax)



let makeLines img (x0, y0, xMax, yMax) = 
    let prevLine    = ref false in
    let currentLine = ref false in
    let lines = Array.make (yMax - y0) false in
    for y = y0 to yMax - 1 do
        currentLine := false;
        for x = x0 to xMax - 1 do
            currentLine := !currentLine || ((Sdlvideo.get_pixel_color img x y)
            = Sdlvideo.black);
        done;
        if !currentLine <> !prevLine then
             (
                lines.(max (y - 1) 1) <- true;
                SDLUtils.drawLineAt img y xMax (0, 0, 255);
             );

        prevLine := !currentLine;
    done;
    lines


let detect (x0, y0, xMax, yMax) src dst network =
    let str = ref "" in
    let imageArray = Array.make_matrix (yMax - y0) (xMax - x0) false in
    let lines = makeLines src(x0, y0, xMax, yMax)  in
    let previousy = ref 0 in
    let previousx = ref 0 in
    for y = y0 to yMax - 1 do 
        for x = y0 to xMax - 1 do
            if Sdlvideo.get_pixel_color dst x y = Sdlvideo.black then    
                put_pixel dst x y (Sdlvideo.get_pixel_color src x y);
                let (localYMin, localYMax) = getMinMax lines y (y0, yMax) in 
                if not imageArray.(y).(x) then
                    let (xmin,ymin,xmax,ymax) = findAdjacent (x, y) (src, dst) 
                        imageArray (localYMin, localYMax) (x0, y0, xMax, yMax) 
                    in 
                    if ymin - !previousy > 5 then
                        str := !str ^ "\n";
                    previousy := ymin;
                    if xmin - !previousx > 9 then
                        str := !str ^ " ";
                    previousx := xmin;
                    let glyph = 
                        Sdlvideo.create_RGB_surface_format 
                            src [] (xmax - xmin) (ymax - ymin)
                    in
                    SDLUtils.imageiter (fun i j ->
                        let px = 
                            Sdlvideo.get_pixel_color 
                                src (xmin + i) (ymin + j) in
                        Sdlvideo.put_pixel_color glyph i j px
                    ) glyph;
                    let s = Network.input_matrix_size in
                    let resized = Resize.resize glyph (s, s) in
                    let (w,h) = SDLUtils.get_dims resized in
                    let mat = Array.make (s*s) (-1.) in
                    for j  = 0 to w - 1 do
                        for i = 0 to h - 1 do 
                            if Sdlvideo.get_pixel_color resized i j =
                                Sdlvideo.black then 
                                mat.(i + j*s) <- 1.;
                        done;
                    done;
                    let out = network#feed mat in
                    str := !str ^ (String.make 1 (FontUtils.from_binary out))
        done
    done;
    !str

