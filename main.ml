open Dmc
open Dmc.Syntax
open Dmc.Logic
open Dmc.DigitalPlane 
open Image
      
let split_extension filename =
  try 
    let i = String.rindex filename '.' in
    (String.sub filename 0 i,String.sub filename (i+1) (String.length filename - i - 1))
  with _ -> (filename,"");;

let _ =
  let imagename = Sys.argv.(1) in
  let rgbimg = ref (load_image imagename) in
  let model = model_of_image (!rgbimg) in
  let env = ref Env.empty in
  let (channel,dst) = try (open_in Sys.argv.(2),Some Sys.argv.(3)) with _ -> draw_image (!rgbimg);(stdin,None) in
  let lexbuf = Lexing.from_channel channel in
  let exit = ref false in
  let counter = ref 0 in
  let refresh () =
    match dst with
      None -> draw_image (!rgbimg)
    | Some filename -> 	    
      let realfilename = if !counter=0 then filename else 
	  let (name,ext) = split_extension filename in
	  Printf.sprintf "%s_%d.%s" name (!counter) ext in
      save_image (!rgbimg) realfilename in
  while not (!exit) do
    try
      let syntax = Parser.main Lexer.token lexbuf in
      match syntax with
	PAINT (color, fsyntax) ->
	  let formula = formula_of_fsyntax (!env) fsyntax in
	  let points = check model formula in
	  let img2 = draw_image_points (!rgbimg) points (Color.color_parse color) in
	  rgbimg := img2;
	  refresh ()
      | LET (ide,formalargs,fsyntax) -> 
	env := Env.add ide (fun_of_decl ide (!env) formalargs fsyntax) (!env)
      | RESET ->
	rgbimg := load_image imagename;
	counter := !counter + 1;
	refresh ()
    with
      Lexer.Eof -> exit := true
    | exn -> 
      let msg = Printexc.to_string exn in
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      Printf.printf "line %d, character %d, token %s: %s\n%!" line cnum tok msg
  done
