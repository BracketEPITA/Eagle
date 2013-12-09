
let resize img new_width new_height = 
  
    let (w,h)   = SDLUtils.get_dims img in
    let tmp     = Sdlvideo.create_RGB_surface_format img [] new_width new_height in
    
    let tmpArr  = Array.make (new_width * new_height) (0, 0, 0) in
    let pixel   = Array.make (w*h) (0, 0, 0) in
    
    let i = ref 0 in
    for y = 0 to new_height - 1 do
         for x = 0 to new_width - 1 do
            pixel.(!i) <- Sdlvideo.get_pixel_color img x y;
            incr i
        done
     done;
    
    let x_ratio = (w lsl 16) / new_width  + 1 in 
    let y_ratio = (h lsl 16) / new_height + 1 in
  
    for i = 0 to new_height - 1 do
        for j = 0 to new_width - 1 do
        let x2 = ((j * x_ratio) lsr 16) in
            let y2 = ((i * y_ratio) lsr 16) in
            tmpArr.((i * new_width) + j) <- pixel.((y2 * new_width) + x2)
        done
    done;
    
    for y = 0 to new_height - 1 do
         for x = 0 to new_width - 1 do
            Sdlvideo.put_pixel_color tmp x y tmpArr.(y * new_width + x)
        done
     done;
    tmp

let resize img  (nw,nh) =
    let (w, h) = SDLUtils.get_dims img in
    
    let tmp         = Sdlvideo.create_RGB_surface_format img [] nw nh in
    let (r_x, r_y)  = (float_of_int w /. float_of_int nw, float_of_int h /. float_of_int nh) in

    for y = 0 to nh - 1 do
        for x = 0 to nw - 1 do
            let px = (int_of_float (r_x *. float_of_int x)) in
            let py = (int_of_float (r_y *. float_of_int y)) in
            Sdlvideo.put_pixel_color tmp x y (Sdlvideo.get_pixel_color img px
            py)
        done
    done;
    tmp
