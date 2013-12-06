(* Dimensions d'une image *)
let get_dims img =
	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let modulo x n = x -. float_of_int (int_of_float (x/.n)) *. n

let half_pi = 1.570796326
let int = int_of_float

let half (x,y) = (x / 2, y / 2)

let rotate theta0 img =
    let theta = mod_float theta0 half_pi in
    let (w0,h0) = SDLUtils.get_dims img in
    let (w,h) = (
        int ((float h0 *. sin theta) +. (float w0 *. cos theta)), 
        int ((float h0 *. cos theta) +. (float w0 *. sin theta))
    ) in
    let (cx0,cy0) = half (w0,h0) in
    let (cx, cy)  = half (w,h) in
    let dst = Sdlvideo.create_RGB_surface_format img [] w h in
    let put = Sdlvideo.put_pixel_color dst in
    let get = Sdlvideo.get_pixel_color img in
    let in_bounds i m = i >= 0 && i < m in 
    let in_bounds (x,y) (w,h) = in_bounds x w && in_bounds y h in
	for i = 0 to w - 1 do
	    for j = 0 to h - 1 do
	        put i j (255,255,255);
    	    let ii = float (i - cx) in
            let jj = float (j - cy) in
            let x = (int (ii *. cos theta0 -. jj *. sin theta0)) + cx0 in
            let y = (int (ii *. sin theta0 +. jj *. cos theta0)) + cy0 in
		    if in_bounds (x,y) (w0,h0) then put i j (get x y);
	    done
	done; dst

let rotation img teta =
    let (w_orig,h_orig) = get_dims img in
    let (w,h) = (
	int_of_float(
	    float_of_int h_orig*.sin(modulo teta half_pi)+.
	    float_of_int w_orig*.sin(half_pi-.modulo teta half_pi)
	),
        int_of_float(
            float_of_int h_orig*.cos(modulo teta half_pi)+.
            float_of_int w_orig*.cos(half_pi-.modulo teta half_pi)
        )
    ) in
    let cx = (w-1) / 2 and cy = (h-1) / 2 in
    let dst = Sdlvideo.create_RGB_surface_format img [] w h in
	for i=0 to w-1 do
	    for j=0 to h-1 do
	    	begin
	            Sdlvideo.put_pixel_color dst i j (255,255,255);
    	            let ii = float_of_int (-cx + i) and jj = float_of_int(-cy + j) in
		    let x = (
                        int_of_float(ii*.cos(teta)) -
                        int_of_float(jj*.sin(teta))+3*cx/4
                    ) in
                    let  y = (
                        int_of_float(ii*.sin(teta)) + 
                        int_of_float(jj*.cos(teta))+6*cy/7
                    ) in
		        if x>=0 && x<w_orig && y>=0 && y < h_orig then
			    Sdlvideo.put_pixel_color dst i j (Sdlvideo.get_pixel_color img x y); 
		end
	    done
	done; dst


