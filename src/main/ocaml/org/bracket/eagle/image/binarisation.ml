let (+.=) = Math.Universe.(+.=)

let get_global_threshold img = 
	let (w,h) = SDLUtils.get_dims img in
	let sum = ref 0. in 
	for j = 0 to h-1 do
       	for i = 0 to w-1 do
            ignore (sum +.= SDLUtils.level (Sdlvideo.get_pixel_color img i j));
        done;
	done; 
	!sum /. float_of_int (w*h) 
	
let binarize img dst threshold = 
    SDLUtils.imageiter (
        fun x y -> Sdlvideo.put_pixel_color dst x y (
            let color = Sdlvideo.get_pixel_color img x y in
            if SDLUtils.level color < threshold then 
                (0,0,0) 
            else 
                (255,255,255)
        )
    ) img
