

let hough img =
	let (w,h) 			= SDLUtils.get_dims img in
	let pi 				= acos (-1.) 			in
	let min_Angle 		= -15. *. pi /. 180. 	in
	let max_Angle 		= 15.  *. pi /. 180.	in
	let step			= pi /. 1000.			in
	let range 			= int_of_float ((abs_float min_Angle +. abs_float max_Angle) /. step) in
	let diag 			= int_of_float (sqrt (float_of_int (w*w + h*h))) in 

	let accumulator 	= Array.make_matrix diag range 0 in
	let current_max 	= ref 0 in
	let current_angle 	= ref 0. in 

	for y = 0 to h-1 do
    	for x = 0 to w-1 do
      		if (Sdlvideo.get_pixel_color img x y) = (0,0,0) then (
      			(* Direct orientation *)
				let theta = ref max_Angle in
				while (!theta > min_Angle) do 
	  	
				  	let r = int_of_float(float x *. cos (!theta) +. float y *. sin (!theta)) in
				  	if r >= 0 then (

					    let theta_acc = int_of_float(!theta +. max_Angle /. step) in
					    accumulator.(r).(theta_acc) <- 1+ accumulator.(r).(theta_acc);
				
					    if (!current_max < accumulator.(r).(theta_acc)) then (
							current_max := accumulator.(r).(theta_acc);
							current_angle := !theta;
					      )
			  		);
	  				theta := !theta -. step;
				done
      		)
    	done
  	done;
	(-. !current_angle)
