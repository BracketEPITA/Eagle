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

let foi x = float_of_int x
let iof x = int_of_float x

let level (r, g, b) = iof(0.3 *. foi(r) +. 0.59 *. foi(g) +. 0.11 *. foi(b))
let color2grey (r, g, b) = let c = level (r, g, b) in (c, c, c)

let image2grey src dst = 
    let (w, h) = get_dims src in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            Sdlvideo.put_pixel_color dst x y (color2grey
            (Sdlvideo.get_pixel_color src x y ))
        done
    done



let makeMean img =
    let meanOfLine = ref 0 in
    let (w, h) = get_dims img in
    let meanArray = Array.make_matrix w h 0 in
    for x = 0 to w - 1 do
        meanOfLine := 0;
       for y = 0 to h - 1 do
           let (r, g, b) = Sdlvideo.get_pixel_color img x y in
           meanOfLine := !meanOfLine + r;
           meanArray.(x).(y) <- !meanOfLine;
       done;
    done;
    meanArray


let getPixel img x y w h m =
    if (x >= w || x < 0 || y >= h || y < 0) then 0
    else m.(x).(y)

let putPixel img x y w h (r, g, b) =
    if x < 0 || x >= w || y < 0 || y >= h then ()
    else Sdlvideo.put_pixel_color img x y (r, g, b)

let getThreshold img x y i j w h m =
    (getPixel img x y w h m) + (getPixel img i j w h m) -
    (getPixel img i y w h m) - (getPixel img x j w h m)

let makeBin img dst =
    let (w, h) = get_dims img in
    let meanArray = makeMean img in
    let size = 16 in
    for x = 0 to w / size do
        for y = 0 to h / size do
            let threshold = getThreshold img (x * size) (y * size)
            (x * (size + 1)) (y * (size + 1)) w h meanArray in
            for _x = 0 to 16 do
                for _y = 0 to 16 do
                    let pixel = getPixel img (x * size + _x) (y * size + _y) w
                    h meanArray in
                    if pixel < threshold then 
                        putPixel dst (x *size +_x) (y * size + _y) w h (0, 0, 0)
                    else
                        putPixel dst (x *size +_x) (y * size + _y) w h (255, 255, 255)
                done;
            done;
        done;
    done


let show img dst =
    let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
               
let main () =
    begin
        if Array.length (Sys.argv) < 2 then
            failwith "Il manque le nom du fichier!";
        sdl_init ();
        
        let img = Sdlloader.load_image Sys.argv.(1) in
        let (w,h) = get_dims img in
        let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
            show img display;
            wait_key ();
            let dst = Sdlvideo.create_RGB_surface_format img [] w h in
            makeBin img dst;
            show dst display;
            wait_key ();

            exit 0
    end
       
let _ = main ()
