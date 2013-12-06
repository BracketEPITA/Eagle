let resize img new_witdh new_height = 
  let (w,h) = SDLUtils.get_dims img in 
  let tmp = Sdlvideo.create_RGB_surface_format img [] new_witdh new_height in
  let x_ratio = int_of_float((w lsl 16) / new_width) +1 in 
  let y_ratio = int_of_float((h lsl 16) / new_height) + 1 in
  for i = 0 to new_height do
    for j = 0 to new_width do
      let x2 = ((j*x_ratio) lsr 16) in
      let y2 = ((i*y_ratio) lsr 16) in
      Sdlvideo.put_pixel_color tmp i j (Sdl.get_pixel_color img x2 y2)
    done
  done;
tmp
