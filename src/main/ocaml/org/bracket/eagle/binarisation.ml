et get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let level (r,g,b) = (0.3*.float_of_int r +. 0.59*.float_of_int g
+.0.11*.float_of_int b)

let color2grey (r,g,b) = let l = int_of_float (level (r,g,b)) in (l,l,l)

let imageiter f img = let (w,h) = get_dims img in
for j=0 to h-1 do
        for i=0 to w-1 do
                f i j
        done
done


let image2grey img dst = imageiter (fun x y -> Sdlvideo.put_pixel_color dst x y (color2grey(Sdlvideo.get_pixel_color img x y))) img


let seuil img = 
	let (w,h) = get_dims img in
	let sum = ref 0. in 
		for j = 0 to h-1 do
       			for i=0 to w-1 do
                		sum:= !sum +. (level (Sdlvideo.get_pixel_color img i j));
			done
		done; 
		!sum /. float_of_int (w*h) 
	

let binarisation img dst = let seuil = seuil img in imageiter (fun x y -> Sdlvideo.put_pixel_color dst x y (if(level(Sdlvideo.get_pixel_color img x y)) < seuil then (0,0,0) else (255,255,255))) img



let makelistofblack img = let l = ref [] and seuil = seuil img in imageiter (fun x y -> if(level(Sdlvideo.get_pixel_color img x y) > seuil) then l:= (x,y)::!l) img  
