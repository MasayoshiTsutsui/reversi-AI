open Array
open Color
open Command


type board = color array array

let init_board () =
  let board = Array.make_matrix 10 10 none in
    for i=0 to 9 do
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if board.(i).(j) = color then
      r
    else
      [] in
    f (di,dj) (i,j) []



let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with
      [] -> false
    | _  -> true

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && is_effective board color (i,j)


let doMove board com color =
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = flippable_indices board color (i,j) in
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in
	let _  = board.(i).(j) <- color in
	  board

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)


let valid_moves board color =
  let ls = [1;2;3;4;5;6;7;8] in
  List.filter (is_valid_move board color)
    (mix ls ls)

let count_valid_moves board color = (*盤面と色を渡すと、有向手の数を返す*)
  List.length (valid_moves board color)

(*let play board color =
  let ms = valid_moves board color in
    if ms = [] then
      Pass
    else
      let k = Random.int (List.length ms) in
      let (i,j) = List.nth ms k in
	Mv (i,j)
*)
let index_of_min list = (*リスト内で最小値があるインデックスを返す*)
  let rec index_of_min_sub l current_i current_min ans_i =
    match l with
    | [] -> ans_i
    | y :: ys -> if y < current_min then index_of_min_sub ys (current_i+1) y current_i
                                    else index_of_min_sub ys (current_i+1) current_min ans_i
  in
  let (top_of_list :: _) = list in
  index_of_min_sub list 0 top_of_list 0


exception Out_of_range

let rec slice list index = (*リストから指定されたインデックスの要素を取得*)
  match list with
  | [] -> raise Out_of_range
  | y :: ys -> if index = 0 then y else slice ys (index-1)

let copy_board board = (*ボードは二重配列のため、ただのcopyだとうまくいかない*)
  Array.map Array.copy board

let play board color =
  let ms = valid_moves board color in
    if ms = [] then
      Pass
    else
      let ocolor = opposite_color color in
      let next_boards = List.map (fun (i,j) -> doMove (copy_board board) (Mv (i,j)) color) ms in (*自分が一手打った後の盤面のリスト*)
      let o_valid_moves_num = List.map (fun b -> count_valid_moves b ocolor) next_boards in (*一手先の盤面での相手の有向手数のリスト*)
      let index = index_of_min(o_valid_moves_num) in (*o_valid_moves_numのリストの要素の中で最小のもののインデックス*)
      let (i,j) = List.nth ms index in
      let unlikable_move_for_o = Mv (i,j) in (*相手の有向手数が最も少なくなるような一手*)
      unlikable_move_for_o

let count board color =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1
      done
    done;
    !s


let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do
    print_int j; print_string "|";
    for i=1 to 8 do
      print_color (board.(i).(j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"


let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = count board black in
  let wc = count board white in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board