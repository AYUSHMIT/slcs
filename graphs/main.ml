open Csmc
open Csmc.GraphLogic
open Csmc.GraphMC
open Graphs 

let props = ["hidden";"exit";"safe";"authorization";"contaminated";"decontamination"]

let eval prop (x,y) = 
  match prop with
  | "exit" -> false (* x = 5 && y = 7 *)
  | "safe" -> (* (x <> 5 || y <> 7) && *) (x <> 5 || y <> 6) && (x < y || (x = y && y = 7) )
  | "authorization" -> x=5 && y = 6
  | "contaminated" -> x > y
  | "decontamination" -> x = y && y != 7
  | "hidden" -> (x<3 && y > 3) || (x > 3 && y < 3)
  | _ -> false

let twocols s = (Printf.sprintf "%s!50" s,Printf.sprintf "%s!20" s)

let colmap prop =
  match prop with
    "exit" -> twocols "black"
  | "safe" -> twocols "green"
  | "authorization" -> twocols "cyan"
  | "contaminated" -> twocols "red"
  | "decontamination" -> twocols "blue"
  | "hidden" -> ("white","white")
  | _ -> ("black","white")
   
let test p opt = if p then opt else []

let fanout (x,y) = 
  if eval "hidden" (x,y) then [] 
  else List.filter (fun p -> not (eval "hidden" p)) 
    (test ((x <> 5) && ((x<>6)||(y<>6))) [(x+1,y);(x-1,y)])
    @(test (x=5 && y=6) [(x+1,y);(x,y-1)])
    @(test ((x=6) && (y = 6)) [(x+1,y)])
    @(test ((x <> 5 || y <> 5) && (((x = 3 && y < 5) || (x = 8 && y <> 5) || x=5))) [(x,y+1)]) 

let arcs = mkgrid_arcs (1,1) (8,7) fanout
      
let (nodes_props,arcs) = (np_of_arcs_props_eval arcs props eval,arcs)
     
let rec tikz_of_nodes_props nodes_props points colmap points_cols =
  match nodes_props with
    [] -> ""
  | ((x,y),props)::xs -> 
    let (fc,bc) =
      if PSet.mem (x,y) points then points_cols
      else
	match props with
	  prop::props -> colmap prop
	| _ -> ("black","white") in
    Printf.sprintf "  \\node [circle,draw=%s,fill=%s,thick,inner sep=0pt,minimum size=4mm] at (%d*0.8,%d*0.8) (nodeX%dY%d) {};\n%s" 
      fc bc x y x y (tikz_of_nodes_props xs points colmap points_cols)

let rec tikz_of_arcs arcs =
  match arcs with
    [] -> ""
  | ((sx,sy),(dx,dy))::xs -> Printf.sprintf "  \\draw[->] (nodeX%dY%d) -- (nodeX%dY%d);\n%s" sx sy dx dy (tikz_of_arcs xs)

let tikz_of_result (nodes_props,arcs) points colmap (points_fc,points_bc) =
  Printf.sprintf "\\begin{tikzpicture}{}\n%s\n%s\\end{tikzpicture}" 
    (tikz_of_nodes_props nodes_props points colmap (points_fc,points_bc)) 
    (tikz_of_arcs arcs)

let _ =
  let model = Graphs.model_of_graph nodes_props arcs in
  let env = ref Env.empty in
  let lexbufs = ref [Lexing.from_channel (open_in Sys.argv.(1))] in
(*  let (lexbufs,outstream) =
    match Array.length Sys.argv with
      1 -> (ref [Lexing.from_channel stdin],stdout)
    | 2 -> (ref [Lexing.from_channel (open_in Sys.argv.(1)); Lexing.from_channel stdin],stdout)
    | _ -> (ref [Lexing.from_channel (open_in Sys.argv.(1))],open_out Sys.argv.(2)) in *)
  let counter = ref 1 in
  while (!lexbufs) <> [] do    
    let lexbuf = List.hd (!lexbufs) in
    try
      let syntax = Parser.main Lexer.token lexbuf in
      match syntax with
	CHECK fsyntax ->
	  let formula = formula_of_fsyntax (!env) fsyntax in
	  let points = check model formula in
	  let outstream = open_out (Printf.sprintf "%s-%d-out.tex" Sys.argv.(2) (!counter)) in
	  Printf.fprintf outstream "%s\n%!" (tikz_of_result (nodes_props,arcs) points colmap ("orange!50","orange!20"));
	  close_out outstream;
	  counter := (!counter) + 1
      | LET (ide,formalargs,fsyntax) -> 
	env := Env.add ide (fun_of_decl ide (!env) formalargs fsyntax) (!env)
    with
      Lexer.Eof -> lexbufs := List.tl (!lexbufs)
    | exn -> 
      let msg = Printexc.to_string exn in
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      Printf.printf "line %d, character %d, token %s: %s\n%!" line cnum tok msg
  done

