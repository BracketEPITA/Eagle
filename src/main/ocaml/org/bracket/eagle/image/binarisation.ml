let foi x = float_of_int x
let iof x = int_of_float x

let at img x y = 
    let (r, g, b) = Sdlvideo.get_pixel_color img x y in
    iof ((0.3 *. foi r) +. (0.59 *. foi g) +. (0.11 *. foi b))

let slowMean img x y integral =
    let (w, h)  = SDLUtils.get_dims img in
    
    let min_y   = max 0 (y - w/2)       in
    let max_y   = min (h - 1) (y + w/2) in
    let min_x   = max 0 (x - w/2)       in
    let max_x   = min (h - 1) (x + w/2) in

    let topright = integral.(min_y).(max_x) in
    let botleft  = integral.(max_y).(min_x) in
    let topleft  = integral.(min_y).(min_x) in
    let botright = integral.(max_y).(max_x) in

    let average = (topright + botleft - topleft - botright) / (w * w) in

    let acc = ref 0 in
    for y = min_y to max_y do
        for x = min_x to max_x do
            acc := !acc + at img x y
        done
    done;
    let average2 = !acc / w * w in

    Printf.printf "\n%d %d %d" (w * w) (topright + botleft - topleft - botright) !acc;
    Printf.printf "\nold : %d & new : %d" average2 average;


    average

let deviation img x y =
  let cur = at img x y                in
  let d_x = at img (max 0 (x - 1)) y  in
  let d_y = at img x (max 0 (y - 1))  in
  cur - d_x - d_y


let threshold img x y k integral =
  let norm_dev = 1. +. k *. (((((float) (deviation img x y))) /. 128.) -. 1.) in

  iof ( foi (slowMean img x y integral) *. norm_dev)

let naiveSauvola img dst k =
    let (w, h) = SDLUtils.get_dims img in
    let integral = Array.make_matrix h w 0 in
    
    let acc_x = ref 0 in
    let acc_y = ref 0 in

    for y = 0 to h - 1 do
        acc_y := !acc_y + at img 0 y;
        acc_x := !acc_y;
        for x = 0 to w - 1 do
            acc_x := !acc_x + at img x y;
            integral.(y).(x) <- !acc_x;
        done
    done;
    

    for y = 0 to h - 1 do
        Printf.printf "Progress : %d/%d \n%!" y h;
        for x = 0 to w - 1 do
            let value = (at img x y) < (threshold img x y k integral) in
            Sdlvideo.put_pixel_color dst x y (if value then (0, 0, 0)
            else (255, 255, 255))
        done
    done

let abin img dst =
    let (w, h) = SDLUtils.get_dims img in
    let intImg = Array.make_matrix w h 0 in
    let s = 80   in
    let t = 60  in

    for i = 0 to w - 1 do
    let sum = ref 0 in
        for j = 0 to h - 1 do
            sum := !sum + at img i j;
            if i = 0 then
                intImg.(i).(j) <- !sum
            else
                intImg.(i).(j) <- intImg.(i - 1).(j) + !sum
        done
    done;

    for i = 0 to w - 1 do
        for j = 0 to h - 1 do
            let x1      = min (max (i - s / 2) 1) (w - 1) in
            let x2      = min (max (i + s / 2) 1) (w - 1) in
            let y1      = min (max (j - s / 2) 1) (h - 1) in
            let y2      = min (max (j + s / 2) 1) (h - 1) in
            let count   = (x2 - x1) * (y2 - y1) in
            let sum     = intImg.(x2).(y2) - intImg.(x2).(y1 - 1) - intImg.(x1 - 1).(y2) + intImg.(x1 - 1).(y1 - 1) in

            if (at img i j * count) <= (sum * (100 - t) / 100) then
                Sdlvideo.put_pixel_color dst i j (0, 0, 0)
            else
                Sdlvideo.put_pixel_color dst i j (255, 255, 255)
        done
    done


let (+.=) = Math.Universe.(+.=)

let get_global_threshold img = 
    let (w,h) = SDLUtils.get_dims img in
    let sum = ref 0. in 
    for j = 0 to h-1 do
            for i = 0 to w-1 do
                ignore (sum +.= SDLUtils.level (Sdlvideo.get_pixel_color img i j))
            done
    done; 
    !sum /. float_of_int (w*h) 
    

let  split_image img = 
    let (w, h)   = SDLUtils.get_dims img in
    let (nw, nh) = (int_of_float (sqrt (float_of_int w)), int_of_float (sqrt (float_of_int h))) in
        
    let tab = Array.create (nw * nh) (-1, -1, -1, -1) in
    for i=0 to nh do
        for j=0 to nw do
            let index = i * nh + j in 
            tab.(index) <- (j * nw, i*nh , (j+1)* nw, (i+1)*nh)
        done
    done; tab


let get_local_treshold img = 
    
    let (w, h)   = SDLUtils.get_dims img in
    let (nw, nh) = (int_of_float (sqrt (float_of_int w)), int_of_float (sqrt (float_of_int h))) in

    let tab = split_image img in
    let tab_of_treshold = Array.create (nw*nh) 127. in
    for i = 0 to Array.length tab - 1 do
        let (x0, y0, xmax, ymax) = tab.(i) in
        let sum =ref 0. in

        for y = y0 to ymax - 1 do
           for x = x0 to xmax - 1 do
               ignore (sum +.= SDLUtils.level(Sdlvideo.get_pixel_color img x y))
           done
        done;
        tab_of_treshold.(i) <- !sum /. float_of_int ((xmax - x0) * (ymax - y0))
    done;
    tab_of_treshold


let localBinarize img dst =
    let tab = split_image img                       in
    let tab_of_treshold = get_local_treshold img    in
    let (width, heigth) = SDLUtils.get_dims img     in

    for i = 0 to Array.length tab - 1 do
        let (x0, y0, xmax, ymax) = tab.(i) in 
        for y = y0 to ymax do
            for x = x0 to xmax do
                if x < width && y < heigth && y > 0 && x > 0 then
                    if SDLUtils.level (Sdlvideo.get_pixel_color img x y) < tab_of_treshold.(i)
                        then Sdlvideo.put_pixel_color dst x y (0,0,0)
                    else
                        Sdlvideo.put_pixel_color dst x y (255,255,255) 
            done
        done
    done

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

