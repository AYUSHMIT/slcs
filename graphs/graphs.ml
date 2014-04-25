open Csmc.GraphLogic
open Csmc.GraphMC

let model_of_graph (nodes_props : ((int * int) * (string list)) list) arcs = 
  let add_node tbl idx n =
    if Hashtbl.mem tbl idx 
    then Hashtbl.replace tbl idx (PSet.add n (Hashtbl.find tbl idx))
    else Hashtbl.add tbl idx (PSet.singleton n) in
  let get_nodes tbl idx = if Hashtbl.mem tbl idx then Hashtbl.find tbl idx else PSet.empty in
  let l = List.length nodes_props in
  let f = Hashtbl.create l in
  let b = Hashtbl.create l in
  let c = Hashtbl.create (3 * (List.length nodes_props) (* TODO: improve this estimate *) ) in
  List.iter (fun (s,t) -> 
    add_node f s t;
    add_node b t s;
  ) arcs;
  List.iter (fun (n,props) -> (List.iter (fun prop -> add_node c prop n) props)) nodes_props;
  let post = fun s -> get_nodes f s in
  { space = 
      { 
	points = List.fold_left (fun res (n,_) -> PSet.add n res) PSet.empty nodes_props;
	post = post;
	pre = (fun t -> get_nodes b t);
	clos = (fun p -> PSet.fold (fun el res -> PSet.union (post el) res) p p)
      };
    eval = (fun n -> get_nodes c n)
  }     
    

let rec filtermaybe l =
  match l with
    [] -> []
  | None::xs -> filtermaybe xs
  | (Some x)::xs -> x::(filtermaybe xs)

let insert x l =
  if List.mem x l then l else x::l

let np_of_arcs_props_eval : (((int * int) * (int * int)) list) -> (string list) -> (string -> (int * int) -> bool) -> 
  ((int * int) * (string list)) list = 
  fun arcs props eval ->
    let nodes = List.fold_left (fun acc ((sx,sy),(dx,dy)) -> insert (sx,sy) (insert (dx,dy) acc)) [] arcs in
    let props node =
      filtermaybe (List.map (fun prop -> if eval prop node then Some prop else None) props) in
    (List.map (fun node -> (node,props node)) nodes)

let mkgrid_arcs : (int * int) -> (int * int) -> ((int * int) -> ((int * int) list)) -> (((int * int) * (int * int)) list) =
  fun (xs,ys) (xe,ye) fanout ->
    let rec mkgrid_rec (cx,cy) = 
      (List.map (fun dst -> ((cx,cy),dst)) 
	 (List.filter (fun (x,y) -> xs <= x && x <= xe && ys <= y && y <= ye) (fanout (cx,cy))))@
	match (cx>=xe,cy>=ye) with
	| (true,false) -> mkgrid_rec (xs,cy+1) 
	| (false,true) -> mkgrid_rec (cx + 1,cy) 
	| (false,false) -> mkgrid_rec (cx + 1,cy)
	| (true,true) -> [] in
    mkgrid_rec (xs,ys) 
