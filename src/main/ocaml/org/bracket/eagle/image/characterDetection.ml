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


let drawRectangle img (x0, xMax, y0, yMax) = 
    for y = y0 to yMax do
        for x = x0 to xMax do
            if x = xMax || y = yMax || x = x0 || y = y0 then
                Sdlvideo.put_pixel_color img x y (255, 0, 0)
        done
    done



let findAdjacent x y src dst imageArray yMini yMaxi = 
    let (xMin, xMax, yMin, yMax) = (ref x, ref x, ref y, ref y) in
    let (w, h) = get_dims src in 
    let rec findAdjacentRec x y =
        if x > 0 && x < w && y > 0 && y < h then
            if Sdlvideo.get_pixel_color src x y = Sdlvideo.black then
                if not(imageArray.(y).(x)) then 
                    (
                        imageArray.(y).(x) <- true;
    
                        xMin := min !xMin x;
                        xMax := max !xMax x;

                        yMin := min !yMin y;
                        yMax := max !yMax y;

                        for y = yMini to yMaxi do
                            for x = x - 1 to x + 1 do
                                findAdjacentRec x y
                            done
                        done
                    )
    in 
    if Sdlvideo.get_pixel_color src x y = Sdlvideo.black then
        (
            findAdjacentRec x y;
            drawRectangle dst (!xMin, !xMax, !yMin, !yMax)
        )


let put_pixel = Sdlvideo.put_pixel_color

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

let getMinMax lines y h = (mini lines y, maxi lines y h)

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


let imageRun src dst = 
    let (w, h) = get_dims src in
    let imageArray = Array.make_matrix h w false in
    let lines = makeLines src in
    for y = 0 to h - 1 do 
        for x = 0 to w - 1 do
            if Sdlvideo.get_pixel_color dst x y <> Sdlvideo.red then    
                put_pixel dst x y (Sdlvideo.get_pixel_color src x y);
                let (yMin, yMax) = getMinMax lines y h in 
                if not imageArray.(y).(x) then
                    findAdjacent x y src dst imageArray yMin yMax;
        done
    done

let main () =
    begin
        if Array.length (Sys.argv) < 2 then
            failwith "Il manque le nom du fichier!";
        sdl_init ();
        
        let img = Sdlloader.load_image Sys.argv.(1) in
        let (w,h) = get_dims img in
        let display =
            Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
            show img display;
            wait_key ();
            let dst = Sdlvideo.create_RGB_surface_format img [] w h in
            imageRun img dst;
            show dst display;
            wait_key ();
            exit 0
    end
       
let _ = main ()
