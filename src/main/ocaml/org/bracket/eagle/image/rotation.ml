(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let modulo x n = x -. float_of_int (int_of_float (x/.n)) *. n

let rotation img teta =
  let (w_orig,h_orig) = get_dims img in
  let (w,h) = (int_of_float(float_of_int h_orig*.sin(modulo teta 1.570796326)+.float_of_int w_orig*.sin(1.570796326-.modulo teta 1.570796326)),int_of_float(float_of_int h_orig*.cos(modulo teta 1.570796326)+.float_of_int w_orig*.cos(1.570796326-.modulo teta 1.570796326))) in
  let cx = (w-1) / 2 and cy = (h-1) / 2 in
  let dst = Sdlvideo.create_RGB_surface_format img [] w h in
  for i=0 to w-1 do
    for j=0 to h-1 do
      begin
	Sdlvideo.put_pixel_color dst i j (255,255,255);
          let ii = float_of_int (-cx + i) and jj = float_of_int(-cy + j) in
          let x = (int_of_float(ii*.cos(teta)) - int_of_float(jj*.sin(teta))+3*cx/4) and y = (int_of_float(ii*.sin(teta)) + int_of_float(jj*.cos(teta))+6*cy/7) in
	  if x>=0 && x<w_orig && y>=0 && y < h_orig then
          Sdlvideo.put_pixel_color dst i j (Sdlvideo.get_pixel_color img x y) ; 
        end
    done
  done ; dst


