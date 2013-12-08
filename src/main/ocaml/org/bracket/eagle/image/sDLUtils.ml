let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
let level (r,g,b) = (0.3*.float_of_int r +. 0.59*.float_of_int g
+.0.11*.float_of_int b)

let imageiter f img = let (w,h) = get_dims img in
    for j=0 to h-1 do
        for i=0 to w-1 do
                f i j
        done
    done


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

let drawRectangle img (x0, xMax, y0, yMax) color = 
    for y = y0 to yMax do
        for x = x0 to xMax do
            if x = xMax || y = yMax || x = x0 || y = y0 then
                Sdlvideo.put_pixel_color img x y color
        done
    done

let drawLineAt img y w color =
    for x = 0 to w - 1 do
        Sdlvideo.put_pixel_color img x y color;
    done

let drawEdgeAt img x h color =
  for y = 0 to h - 1 do
    Sdlvideo.put_pixel_color img x y color;
  done
