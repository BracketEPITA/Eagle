let sdl_init () =
    begin
        Sdl.init [`EVERYTHING];
        Sdlevent.enable_events Sdlevent.all_events_mask;
    end
let rec wait_key () =
    let e = Sdlevent.wait_event () in
    match e with
        Sdlevent.KEYDOWN _ -> ()
        | _                -> wait_key ()
        
let show img dst =
    let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
let get_dims img =
    ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)



let main () =
    begin
        if Array.length (Sys.argv) < 2 then
            failwith "Il manque le nom du fichier!";
        sdl_init ();
        let img = Sdlloader.load_image Sys.argv.(1) in
        let (w,h) = get_dims img in
        let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
            
            (* Images surfaces *)
            let denoised  = Sdlvideo.create_RGB_surface_format img [] w h in
            let binarized = Sdlvideo.create_RGB_surface_format img [] w h in
            let detected  = Sdlvideo.create_RGB_surface_format img [] w h in
    
            (* OPerations on images *)
            NoiseReduction.median     img       denoised  1;
            Binarisation.binarisation denoised  binarized;
            let rotated = Rotation.rotation
                binarized
                (Hough.hough binarized) in
            CharacterDetection.imageRun img detected;

            (* Displaying *)
            show img       display; wait_key();
            show denoised  display; wait_key();
            show binarized display; wait_key();
            show rotated   display; wait_key();
            show detected  display; wait_key();


            exit 0
    end

let _ = main ()


