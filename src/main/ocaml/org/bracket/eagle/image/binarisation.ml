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

let threshold img = 
	let (w,h) = get_dims img in
	let sum = ref 0. in 
	for j = 0 to h-1 do
       	    for i=0 to w-1 do
                sum:= !sum +. (level (Sdlvideo.get_pixel_color img i j));
	    done
	done; 
	!sum /. float_of_int (w*h) 
	
let binarisation img dst =
    let threshold = threshold img in 
    imageiter (
        fun x y -> Sdlvideo.put_pixel_color dst x y (
            if(level(Sdlvideo.get_pixel_color img x y)) < threshold then (0,0,0) 
            else (255,255,255)
        )
    ) img



let makeListOfBlack img = 
    let l = ref [] and threshold = threshold img in 
    imageiter (
        fun x y -> if(level(Sdlvideo.get_pixel_color img x y) > threshold) then l:= (x,y)::!l
     ) img  
