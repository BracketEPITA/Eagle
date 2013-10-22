let get_dims img =
    ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let sdl_init () =
    begin
        Sdl.init [`EVERYTHING];
        Sdlevent.enable_events Sdlevent.all_events_mask;
    end

let rec wait_key () =
    let e = Sdlevent.wait_event () in
    match e with
        Sdlevent.KEYDOWN _ -> ()
        | _ -> wait_key ()

let show img dst =
    let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst



let oldPropagate img x y m =
    let xMin = ref x in
    let yMin = ref y in
    let xMax = ref x in
    let yMax = ref y in
    let rec propagate_rec x y =
        if not(x < 0 || x >= Array.length m || y < 0 || y >= Array.length m.(x)) then
            if not(m.(x).(y) || Sdlvideo.get_pixel_color img x y <> (0, 0, 0))
            then (
                m.(x).(y) <- true;
                xMin := min (!xMin) x;
                yMin := min (!yMin) y;
                xMax := max (!xMax) x;
                yMax := max (!yMax) y;
            
                propagate_rec (x + 1) y; 
                propagate_rec (x - 1) y;
                propagate_rec x (y + 1);
                propagate_rec x (y - 1);
                propagate_rec (x + 1) (y - 1);
                propagate_rec (x - 1) (y - 1);
                propagate_rec (x + 1) (y + 1);
                propagate_rec (x - 1) (y + 1);
            )
    in (propagate_rec x y);
    (!xMin, !yMin, !xMax, !yMax)
                            
let propagate img x y averageExp m =
    let xMin = ref x in
    let yMin = ref y in
    let xMax = ref x in
    let yMax = ref y in

    let rec propagate_rec x y =
        if not(x < 0 || x >= Array.length m || y < 0 || y >= Array.length m.(x)) then
            if not(m.(x).(y) || Sdlvideo.get_pixel_color img x y <> (0, 0, 0))
            then (
                m.(x).(y) <- true;
                xMin := min (!xMin) x;
                yMin := min (!yMin) y;
                xMax := max (!xMax) x;
                yMax := max (!yMax) y;
                for i = y - max 1 averageExp to y + max 1 averageExp do
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

let drawLineAt img y w =
    for x = 0 to w - 1 do
        Sdlvideo.put_pixel_color img x y (0, 0, 255);
    done

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
             (
                lines.(max (y - 1) 1) <- true;
                drawLineAt img y w;
             );

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
            let (yMin, yMax) = ((mini lines y), (maxi lines y h)) in
            let height = yMax - yMin in
            let averageExploration = height * 30 /100 in
            Sdlvideo.put_pixel_color dst x y (Sdlvideo.get_pixel_color img x y);
            if not(m.(x).(y)) && Sdlvideo.get_pixel_color img x y = (0, 0, 0)  then
                blocks := (propagate img x y averageExploration m)::(!blocks);
        done;
    done;
    drawRectangles dst !blocks

let oldImageRun img dst = 
    let (w, h) = get_dims img in
    let m = Array.make_matrix w h false in
    let blocks = ref [] in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            Sdlvideo.put_pixel_color dst x y (Sdlvideo.get_pixel_color img x y);
            if not(m.(x).(y)) && Sdlvideo.get_pixel_color img x y = (0, 0, 0)  then
                blocks := (oldPropagate img x y m)::(!blocks);
        done;
    done;
    drawRectangles dst !blocks

