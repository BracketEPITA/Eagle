let get_dims img =
      ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info
      img).Sdlvideo.h)
       
let foi x = float_of_int x
let iof x = int_of_float x

let hough img =
    let (w, h) = get_dims img in
    
    let pi = acos(-1.) in
    let piOver2 = asin(1.) in
    
    (* cos x + sin x max value is sqrt(2)  *)
    let rhoMax = w + h in 
    let thetaMax = (iof(pi *. 100.)) + 1 in

    let thetaByRho = Array.make_matrix rhoMax thetaMax 0 in
    
    let maxTheta  = ref 0. in (* Max found *)
    let bestTheta = ref 0  in (* How much time the current theta was found *)

    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            if (Sdlvideo.get_pixel_color img x y) = (0, 0, 0) then
                (
                    let angle = ref 0. in
                    while !angle <= pi do
                        (* x * cos theta + y * cos theta *)
                        let rho = abs (iof (foi(x) *. cos !angle +. foi(y) *. sin
                        !angle)) in 
               
                        let theta = iof (!angle *. 100.) in
                        thetaByRho.(rho).(theta) <- thetaByRho.(rho).(theta) + 1;
               
                        if !bestTheta < thetaByRho.(rho).(theta) then
                            (
                                bestTheta := thetaByRho.(rho).(theta);
                                maxTheta  := !angle
                            );
                        angle := !angle +. 0.01;
                    done;
                )
        done;
    done;
    !maxTheta -. piOver2

