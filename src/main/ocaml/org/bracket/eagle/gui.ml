let network = ref (Obj.magic None)

(* Structure interface *)
let window =
  ignore (GMain.init ());
  let wnd = GWindow.window
    ~title:"OCR_Brackets"
    ~position:`CENTER
    ~resizable:true
    ~width:1300 ~height:650 () in
  ignore (wnd#connect#destroy GMain.quit);
  wnd

(* Box de la fenetre *)
let boxWin =
  let box = GPack.vbox
        ~spacing:2
        ~packing:window#add () in
   box#set_homogeneous false;
   box

(* Box et barre de menu *)
let menu =
  let box = GPack.hbox
        ~spacing:5
        ~packing:(boxWin#pack ~expand:false) () in
  box#set_homogeneous true;
  box

let toolbar = GButton.toolbar
        ~orientation:`HORIZONTAL
        ~style:`BOTH
        ~packing:(menu#pack ~expand:false) ()

let textToolbar =
  let box = GPack.hbox
    ~spacing:5
    ~packing:(menu#pack ~expand:true) ()in
  box#set_homogeneous true;
  box

let optionToolbar = GButton.toolbar
        ~orientation:`HORIZONTAL
        ~style:`BOTH
        ~packing:(menu#pack ~expand:true) ()


(* Box de l'image et du resultat *)
let boxh =
  let box = GPack.hbox
    ~spacing:5
    ~packing:(boxWin#pack ~expand:true) () in
  box#set_homogeneous true;
  box

(* Affichage de l'image *)
let view =
  let scroll = GBin.scrolled_window
        ~hpolicy:`AUTOMATIC
        ~vpolicy:`AUTOMATIC
        ~packing:boxh#add () in
  let view1 = GPack.vbox
        ~packing:scroll#add_with_viewport () in
view1


(* Affichage du resultat *)
let result =
  let scroll = GBin.scrolled_window
        ~hpolicy:`AUTOMATIC
        ~vpolicy:`AUTOMATIC
        ~packing:boxh#add () in
  let view1 = GPack.vbox
        ~packing:scroll#add_with_viewport () in
view1

(* Affichage text *)
let boxText = GPack.hbox
        ~spacing:5
        ~packing:(boxh#pack ~expand:false) ()
let text =
  let scroll = GBin.scrolled_window
        ~hpolicy:`NEVER
        ~vpolicy:`ALWAYS
        ~packing:boxText#add () in
  let txt = GText.view
        ~wrap_mode:`WORD
        ~packing:scroll#add () in
  txt#misc#modify_font_by_name "Monospace 10";
  txt

                        (* Creation bouton menu *)

let openB = GButton.tool_item ~packing:toolbar#insert ()
let eagl = GButton.tool_item ~packing:toolbar#insert ()
let feedback = GPack.hbox
  ~spacing:5
  ~packing:(textToolbar#pack ~expand:false) ()
let help = GButton.tool_item ~packing:optionToolbar#insert ()
let about = GButton.tool_item ~packing:optionToolbar#insert ()
let quit = GButton.tool_item ~packing:optionToolbar#insert ()

(*Feedback display*)
let text2 =
  let scroll = GBin.scrolled_window
        ~hpolicy:`NEVER
	~vpolicy:`NEVER
        ~packing:feedback#add () in
  let txt = GText.view
        ~wrap_mode:`WORD
        ~packing:scroll#add () in
  txt#misc#modify_font_by_name "Monospace 10";
  txt
(* Button Open *)
let buttonopen =
  let imageFilter = GFile.filter
          ~name:"Image"
          ~patterns:["*.png"; "*.jpeg"; "*.jpg"] () in
  let btn = GFile.chooser_button
        ~title:"OPEN"
        ~action:`OPEN
        ~packing:openB#add () in
  let may_view btn () =
     match btn#filename with
     |Some n ->
          List.iter view#remove view#children;
          List.iter result#remove result#children;
          text#buffer#delete
              ~start:text#buffer#start_iter
              ~stop:text#buffer#end_iter;
	  text2#buffer#delete
	    ~start:text2#buffer#start_iter
	    ~stop:text2#buffer#end_iter;
	  text2#buffer#insert("waiting for some action...");
          ignore (GMisc.image
          ~file: n
          ~packing:view#add());
     |None -> () in
  btn#set_filter imageFilter;
  ignore (btn#connect#selection_changed (may_view btn));
  btn

let path = ref ""
let angle = ref 0.
(*Button = New Image display / final text display / feedback display*)
let eagl_button =
  let btn = GButton.button
    ~stock:`EXECUTE
    ~packing:eagl#add()in
  (*scd image*)
  let display () = 
    match buttonopen#filename with
    |Some n -> (
    	path := n;
    	List.iter result#remove result#children;
    	ignore (GMisc.image
	      ~file: n
	      ~packing:result#add()
	  	))
    |None   -> ()
    in
  (*final text to display*)
  let erase ()=text2#buffer#delete
    ~start:text2#buffer#start_iter
    ~stop:text2#buffer#end_iter in
    let write9 ()=
    	erase();
    	text2#buffer#insert("...Eagle is done ;-)");
    	let img = Sdlloader.load_image !path in
    	let mat = Array.make (15 * 15) 0. in
    	for y = 0 to 14 do
    		for x = 0 to 14 do
    			mat.(y * 15 + x) <- if 
    			(Sdlvideo.get_pixel_color img x y) = (0, 0, 0) then 1. else 0.
    		done
    	done;
    	let out = (!network)#feed mat in
    	let str = String.make 1 (FontUtils.from_binary out) in
    	text#buffer#insert(str) in
		
  
  let action ()=
    text#buffer#delete
    ~start:text#buffer#start_iter
    ~stop:text#buffer#end_iter;
    display();
    write9() in
    
  ignore(btn#connect#clicked ~callback:action);
  btn



(* Button help *)
let help_button =
  let dlg = GWindow.message_dialog
        ~message:"\nRTFM or flee!"
        ~title:"Help"
        ~parent:window
        ~destroy_with_parent:true
        ~use_markup:true
        ~message_type:`QUESTION
        ~position:`CENTER_ON_PARENT
        ~buttons:GWindow.Buttons.ok () in
  let btn = GButton.button
        ~stock:`HELP
        ~packing:help#add () in
  ignore (GMisc.image
        ~stock:`HELP
        ~packing:btn#set_image ());
  ignore (btn#connect#clicked (fun ()->ignore(dlg#run());dlg#misc#hide()));
  btn

(* Button about *)
let about_button =
  let dlg = GWindow.about_dialog
        ~name:"Eagl"
        ~authors:["                            Brackets

Mathieu Franklin
Gaubert Renaud
Adon Remi
Bosma Francois"]
        ~license:"Texte kikoo"
        ~position:`CENTER_ON_PARENT
        ~parent:window
        ~destroy_with_parent:true () in
  let btn = GButton.button
        ~stock:`ABOUT
        ~packing:about#add () in
  ignore (GMisc.image
        ~stock:`ABOUT
        ~packing:btn#set_image ());
  ignore (btn#connect#clicked (fun ()->ignore (dlg#run ());dlg#misc#hide ()));
  btn

(* Button pour quitter *)
let buttonquit =
  let btn = GButton.button
        ~stock:`QUIT
        ~packing:quit#add () in
  ignore (btn#connect#clicked GMain.quit);
  btn

                        (* Fenetre de confirmation pour quitter *)
let confirm _ =
  let dlg = GWindow.message_dialog
    ~title:"Quit"
    ~message:"<b><big>Do you really want to quit ?</big>

      Be Careful :
      You will lose the data that you didn't save !</b>"
    ~parent:window
    ~destroy_with_parent:true
    ~use_markup:true
    ~message_type:`QUESTION
    ~position:`CENTER_ON_PARENT
    ~buttons:GWindow.Buttons.yes_no () in
  let res = dlg#run () = `NO in
  dlg#destroy ();
  res

let startInterface () =
  network := NetworkSerialization.deserialize "network.bin";
  window#show ();
  ignore (window#event#connect#delete confirm);
  GMain.main ()
