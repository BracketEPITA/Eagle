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

let propagate img x y m =
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
                propagate_rec x (y + 1); 
                propagate_rec x (y - 1);
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


let imageRun img dst = 
    let (w, h) = get_dims img in
    let m = Array.make_matrix w h false in
    let blocks = ref [] in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            Sdlvideo.put_pixel_color dst x y (Sdlvideo.get_pixel_color img x y);
            if not(m.(x).(y)) && Sdlvideo.get_pixel_color img x y = (0, 0, 0)  then
                blocks := (propagate img x y m)::(!blocks);
        done;
    done;
    drawRectangles dst !blocks
    
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
            imageRun img dst;
            show dst display;
            wait_key ();
            exit 0
        end
       
let _ = main ()
