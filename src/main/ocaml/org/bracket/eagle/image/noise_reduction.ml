(*dimensions d'une image *)
let get_dims img =
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


let color_add (r1,g1,b1) (r2,g2,b2) = ((r1 + r2),(g1 + g2),(b1 + b2))
 
let color_div (r,g,b) d = ((r / d), (g / d), (b / d))
 
let compare_as_grayscale (r1,g1,b1) (r2,g2,b2) =
  let v1 = (2_126 * r1 +  7_152 * g1 + 722 * b1)
  and v2 = (2_126 * r2 +  7_152 * g2 + 722 * b2) in
  (Pervasives.compare v1 v2)
 
 
let median_value img radius =
  let samples = (radius*2+1) * (radius*2+1) in
  let sample = Array.make samples (0,0,0) in
  fun x y ->
    let i = ref 0 in
    for _x = (x - radius) to (x + radius) do
      for _y = (y - radius) to (y + radius) do
        let v = Sdlvideo.get_pixel_color img _x _y in
        sample.(!i) <- v;
        incr i;
      done;
    done;
 
    Array.sort compare_as_grayscale sample;
    let mid = (samples / 2) in
 
    if (samples mod 2) = 1
    then sample.(mid+1)
    else (color_div (color_add sample.(mid)
                               sample.(mid+1)) 2) 


let median img image radius =
  let width = (Sdlvideo.surface_info img).Sdlvideo.w 
  and height = (Sdlvideo.surface_info img).Sdlvideo.h in

  let _median_value = median_value img radius in 
  for y = 0 to pred height do
    for x = 0 to pred width do
      let color = _median_value x y in
      Sdlvideo.put_pixel_color image x y color;
    done;
  done



(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Filename is missing!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    let image = Sdlvideo.create_RGB_surface_format img [] w h in
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      median img image 3;
      (*on binarise l'image*)
      show image display;
      (*on attend de nouveau une touche*)
      wait_key ();
      exit 0
  end

let _ = main ()
