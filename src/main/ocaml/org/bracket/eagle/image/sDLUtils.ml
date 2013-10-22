
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
