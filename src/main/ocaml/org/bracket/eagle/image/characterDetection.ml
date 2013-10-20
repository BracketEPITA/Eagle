let get_dims img =
    ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
        
let propagate img x y yMini yMaxi m =
    let xMin = ref x in
    let yMin = ref y in
    let xMax = ref x in
    let yMax = ref y in

    let rec propagate_rec x y =
        if x < 0 || x >= Array.length m || y < 0 || y >= Array.length m.(x) then
            ()
        else
            if m.(x).(y) || Sdlvideo.get_pixel_color img x y <> (0, 0, 0) then ()
            else (
                m.(x).(y) <- true;
                xMin := min (!xMin) x;
                yMin := min (!yMin) y;
                xMax := max (!xMax) x;
                yMax := max (!yMax) y;
                propagate_rec (x + 1) y;
                propagate_rec (x - 1) y;
                for i = yMini to yMaxi -1 do
                    propagate_rec x (i);
                    propagate_rec (x + 1) (i);
                    propagate_rec (x - 1) (i);
                done;
            )
    in (propagate_rec x y);
    (!xMin, !yMin, !xMax, !yMax)

let rec drawRectangles img = function
    |[] -> ()
    |(xMin, yMin, xMax, yMax)::t -> (
        for x = xMin to xMax do
            Sdlvideo.put_pixel_color img x yMin (255, 0, 0);        
            Sdlvideo.put_pixel_color img x yMax (255, 0, 0);
        done;
        for y = yMin to yMax do
            Sdlvideo.put_pixel_color img xMin y (255, 0, 0);
            Sdlvideo.put_pixel_color img xMax y (255, 0, 0);
        done;
        drawRectangles img t;
    )

let mini lines y =
    let r = ref y in
    while !r >= 0 && (not lines.(!r)) do
        r := !r - 1;
    done;
    !r

let maxi lines y h =
    let r = ref y in
    while !r < h && (not lines.(!r)) do
        r := !r + 1;
    done;
    !r

let makeLines img = 
    let prevLine    = ref false in
    let currentLine = ref false in

    let (w, h) = get_dims img in 
    let lines = Array.make h false in
    for y = 0 to h - 1 do
        currentLine := false;
        for x = 0 to w - 1 do
            currentLine := !currentLine || ((Sdlvideo.get_pixel_color img x y)
            = (0, 0, 0));
        done;
        if !currentLine <> !prevLine then
             lines.(y - 1) <- true;

        prevLine := !currentLine;
    done;
    lines

let imageRun img dst = 
    let (w, h) = get_dims img in
    let m = Array.make_matrix w h false in
    let lines = makeLines img in
    let blocks = ref [] in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            Sdlvideo.put_pixel_color dst x y (Sdlvideo.get_pixel_color img x y);
            if not(m.(x).(y)) && Sdlvideo.get_pixel_color img x y = (0, 0, 0)  then
                blocks := (propagate img x y (mini lines y) (maxi lines y h) m)::(!blocks);
        done;
    done;
    drawRectangles dst !blocks

let drawLineAt img y w =
    for x = 0 to w - 1 do
        Sdlvideo.put_pixel_color img x y (255, 0, 0);
    done
