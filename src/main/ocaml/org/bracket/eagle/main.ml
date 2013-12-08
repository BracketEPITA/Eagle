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

let ios x = int_of_string x

let matrixLength str = 
    let l = ref 1 in
    for i = 0 to (String.length str) - 1 do
        if str.[i] = ' ' then
            l := !l + 1;
    done;
    !l

let split str = 
    let matrix = Array.make (max 1 (matrixLength str)) "" in (* max for outof
    bounds case *)
    let l = ref 0 in
    for i = 0 to (String.length str) - 1 do
        if str.[i] = ' ' then
            l := !l + 1
        else
            matrix.(!l) <- matrix.(!l) ^ String.make 1 str.[i];
    done;
    matrix
        
let argCheck x words = 
    if Array.length (words) < x then
        raise (Invalid_argument "Not enough args")
        
let apply str dest f = 
    let img = Sdlloader.load_image str in
    let (w,h) = get_dims img in
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    let dst = Sdlvideo.create_RGB_surface_format img [] w h in
    f img dst;
    Sdlvideo.save_BMP dst dest;
    show img display; wait_key ();
    show dst display; wait_key ()

let main () =
    begin
        let words = ref [||] in
        let exitShell = ref false in

        if Array.length Sys.argv > 1 then (
            words := Array.init (Array.length Sys.argv - 1) (fun i ->
                Sys.argv.(i + 1)
            );
        );

        
        let builder = new NetworkFactory.factory in
            builder#add_layer {
                NetworkFactory.size = 2; 
                NetworkFactory.has_bias = true; 
                NetworkFactory.activation = Network.ActivationFunction.sigmoid
            };
            builder#add_layer {
                NetworkFactory.size = 10000;
                NetworkFactory.has_bias = true;
                NetworkFactory.activation = Network.ActivationFunction.sigmoid
            };
            builder#add_layer {
                NetworkFactory.size = 1;
                NetworkFactory.has_bias = false;
                NetworkFactory.activation = Network.ActivationFunction.sigmoid
            };
        let data = Network.new_dataset () in
            Network.add_entry data [|-1.; -1.|] [|0.|];
            Network.add_entry data [| 1.; -1.|] [|1.|];
            Network.add_entry data [|-1.;  1.|] [|1.|];
            Network.add_entry data [| 1.;  1.|] [|0.|];
        let network = ref builder#build in
        let dataset = ref (Obj.magic None) in
        while not !exitShell do
            SDLUtils.sdl_init ();
            (try
                if Array.length Sys.argv > 1 then 
                    exitShell := true
                else (
                    Printf.printf "> ";
                    let line = read_line () in
                    words := split line;
                );
                match !words.(0) with
                    | "nred" -> (
                        argCheck 3 !words;
                        let radius = if Array.length !words >= 4 then 
                            int_of_string !words.(3)
                        else 
                            1 
                        in
                        apply 
                            !words.(1)
                            !words.(2) 
                            (fun img dst ->
                                NoiseReduction.median img dst radius
                            )
                    )
                    | "angle" ->
                        argCheck 2 !words;
                                let img = Sdlloader.load_image !words.(1) in
                                let angle = Newhough.hough img in
                                Printf.printf "Hough angle is %f\n" (angle)

                    | "ang" -> (
                                argCheck 2 !words;
                                let img = Sdlloader.load_image !words.(1) in
                                let angle = Hough.hough img in
                                Printf.printf "Hough angle is %f\n" (-.angle +.
                                0.5)
                    )
                    | "rot" -> (
                        argCheck 4 !words;
                        let img = Sdlloader.load_image !words.(1) in
                        let dst = Rotation.rotate (float_of_string
                        !words.(3)) img in 
                        let (w, h) = get_dims dst in
                        let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF]
                        in show dst display;
                        Sdlvideo.save_BMP dst !words.(2);
                        wait_key ();
                    )
                    | "det" -> (
                        argCheck 7 !words;
                        apply !words.(1) !words.(2) 
                                (CharacterDetection.detect (ios !words.(3), ios
                                !words.(4), ios !words.(5), ios !words.(6)))
                    )
                    | "mknw" -> (
                        network := builder#build
                    )
                    | "trnw" -> (
                        (!network)#set_training_method (BackPropagation.train);
                        (!network)#train ~post:(fun i error ->
                            if i mod 100 = 0 then Printf.printf "epoch %d : error = %f\n" i error;
                        ) data;
                    )
                    | "setv" -> (
                        argCheck 3 !words;
                        let inputs = [|
                            float_of_string !words.(1);
                            float_of_string !words.(2);
                        |] in 
                        let outputs = (!network)#set_values inputs in
                            print_string "outputs = [";
                            Array.iter (fun e -> 
                                Printf.printf "%f " e;
                            ) outputs;
                            print_string "]\n";
                    )
                    | "save" -> (
                        argCheck 1 !words;
                        NetworkSerialization.serialize !words.(1) (!network);
                    )
                    | "load" -> (
                        argCheck 1 !words;
                        network := NetworkSerialization.deserialize !words.(1)
                    )
                    | "initnw" -> (
                        let (net,data) = OCR.init_network () in
                        network := net;
                        dataset := data;
                    )
                    | "chknw" -> (
                        argCheck 1 !words;
                        let rec iter = function
                            | e::l -> Printf.printf "%c\n" (OCR.get_char
                            (!network) e); iter l
                            | [] -> ()
                        in iter (!dataset).Network.inputs
                    )
                    | "mat" -> (
                        let f = fun (i, j) -> j * 3 + i in
                        Matrix.print_matrix print_int (Matrix.init (3,3) f);
                        Matrix.print_matrix print_int (Matrix.init0 (3,3) f)
                        (*let m = [|[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]; [|10; 11; 12;|]|] in
                        let m2 = Matrix.identity 4 in
                        let r = Matrix.mul m m2 in
                        Matrix.print_matrix print_int r;*)
                    )
                    | "spin" -> (
                        argCheck 1 !words;
                        let theta = ref 0. in
                        let img = Sdlloader.load_image !words.(1) in
                        while true do
                            let dst = Rotation.rotate !theta img in
                            let (w,h) = get_dims dst in
                            let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
                            show dst display;
                            theta := !theta +. 0.01;
                        done;
                    )
                    | "mkfont" -> (
                        (*let arial = Sdlttf.open_font
                        "Arial.ttf" 16 in
                        FontUtils.generate_bitmap arial
                        "abcdefghijklmnopqrstuvwxyz"*)
                    )
                    | "exit" -> (
                        exitShell := true
                    )
                    | "help" -> (
                        print_string (
                            "help\n" ^
                            "exit\n" ^
                            "image :\n" ^
                            "    bin <img> <dst> [threshold:auto]\n" ^
                            "    nred <img> <dst> [radius:1]\n" ^
                            "    ang <img> <dst>\n" ^
                            "    rot <img> <dst> <angle>\n" ^
                            "    cdet <img> <dst> [legacy]\n" ^
                            "network :\n" ^
                            "    mknw\n" ^
                            "    trnw\n" ^
                            "    setv <val1> <val2>\n"
                        );

                    )
                    | _ -> Printf.printf "Unknown command \n" 
            with 
            | Not_found -> Printf.printf "Not found\n");
            Sdl.quit ();
        done;

            exit 0
    end

let _ = main ()


