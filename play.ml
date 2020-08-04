open Array
open Color
open Command

type opmove = PMove of move | OMove of move

let string_of_opmove = function
  | PMove mv -> "+" ^ string_of_move mv
  | OMove mv -> "-" ^ string_of_move mv

type hist = opmove list

let string_of_hist x = List.fold_left (fun r a -> string_of_opmove a ^ " " ^ r) "" x
let print_hist x = print_endline (string_of_hist x)

let file = "logbook.gam"


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

let list_next_boards board color ms = (*盤面と色と有向手のリストを渡すと、1つ先の盤面のリストを返す*)
  List.map (fun (i,j) -> doMove (copy_board board) (Mv (i,j)) color) ms

let list_next_move_and_boards board color ms = (*1つ先の盤面と、それに至った一手をペアにして返す. init_surely_winにて使用*)
  List.map (fun (i,j) -> ((i,j), doMove (copy_board board) (Mv (i,j)) color)) ms


let count board color =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1
      done
    done;
    !s

let is_gameset board =
  let s = ref true in
  for i=1 to 8 do
    for j=1 to 8 do
      if board.(i).(j) = none then s := false
    done
  done;
  !s

let is_true boolean = boolean = true

(*盤面と打ち手の色を渡すと、必勝ルートがあるかをbool値で返す*)
let rec surely_win board color =
  match is_gameset board with
  |true -> if count board color > 32 then true else false
  |false -> let ms = valid_moves board color in
            let next_boards = list_next_boards board color ms in
            let ocolor = opposite_color color in
            List.exists is_true (List.map (fun b -> surely_lose b ocolor) next_boards) 
and surely_lose board color =
  match is_gameset board with
  |true -> if count board color < 32 then true else false
  |false -> let ms = valid_moves board color in
            let next_boards = list_next_boards board color ms in
            let ocolor = opposite_color color in
            List.for_all is_true (List.map (fun b -> surely_win b ocolor) next_boards)

type victory_road = Win of (int * int) | Undecidable
exception Ms_Nextboards_mismatch
(*必勝手がある場合、その必勝手につながる最初の一手も一緒に返してほしいので、surely_winをplay関数でいきなり呼ぶのではなく、init_surely_winを呼んで、この関数が内部でsurely_loseを呼ぶ*)
let init_surely_win board color =
  let ms = valid_moves board color in
  let ocolor = opposite_color color in
  let next_boards = list_next_boards board color ms in
  let rec init_surely_iter mvlist nboards = (*msとnext_boardsを渡すと、next_boardsを一つ一つsurely_loseに渡し、必勝手がある場合は、その手をmsから返す関数(msとnext_boardsのインデックスが対応しているから単純なパターンマッチで行ける)*)
    match (mvlist, nboards) with
    | ([], []) -> Undecidable
    | (m::mrest, nb::nbrest) -> if surely_lose nb ocolor then Win m else init_surely_iter mrest nbrest
    | _ -> raise Ms_Nextboards_mismatch
  in
  init_surely_iter ms next_boards

let count_stones board =
  let s = ref 0 in
  for i=1 to 8 do
    for j=1 to 8 do
      if board.(i).(j) != none then s := !s + 1
    done
  done;
  !s

let min_of_list l =
  let rec min_iter l min =
    match l with
    | [] -> min
    | i :: rest -> if i < min then min_iter rest i else min_iter rest min
  in min_iter l 64

let max_of_list l =
  let rec max_iter l max =
    match l with
    | [] -> max
    | i :: rest -> if i > max then max_iter rest i else max_iter rest max
  in max_iter l 0


let idx_of_max l = (*数値リストの最大値のインデックスを返す*)
  let rec idx_iter l cur_idx max max_idx =
    match l with
    | [] -> max_idx
    | i :: rest -> if i > max then idx_iter rest (cur_idx+1) i cur_idx else idx_iter rest (cur_idx+1) max max_idx 
  in idx_iter l 0 0 0

let increase_my_valids next_boards color ms = (*2手先の盤面における自分の有効手数を評価関数に据え、minmax法で評価した際の最善手を返す*)
  let ocolor = opposite_color color in
  let boards_2turns_ahead = List.map (fun b -> list_next_boards b ocolor (valid_moves b ocolor)) next_boards in (*2手先の盤面のリスト。盤面配列の2重リストになっていることに注意*)
  let valuation_of_nextboards = List.map (fun blist -> min_of_list (List.map (fun b -> count_valid_moves b color) blist)) boards_2turns_ahead in (*2手先の盤面(自分の手番)における有向手数のリストを最内のList.mapで求め、敵がその中で、自分の有向手数がもっとも少ない手を打つと想定。よって1手先の盤面の評価値は、その盤面の更に1手先における自分の有向手数の最小値となる*)
  let best_mv_idx = idx_of_max valuation_of_nextboards in (*valuation_of_nextboardsのなかで評価値の最も高い値のインデックス*)
  let best_move = List.nth ms best_mv_idx in (*msのインデックスと、next_boardsのインデックスが対応しているため、このようにできる*)
  best_move

let reference_board =
  let b = Array.make_matrix 10 10 0 in
  b.(1).(1) <- 45; b.(1).(8) <- 45; b.(8).(1) <- 45; b.(8).(8) <- 45;
  b.(1).(2) <- -11; b.(1).(7) <- -11; b.(2).(1) <- -11; b.(2).(8) <- -11; b.(7).(1) <- -11; b.(7).(8) <- -11; b.(8).(2) <- -11; b.(8).(7) <- -11;
  b.(1).(3) <- 4; b.(1).(6) <- 4; b.(3).(1) <- 4; b.(3).(8) <- 4; b.(6).(1) <- 4; b.(6).(8) <- 4; b.(8).(3) <- 4; b.(8).(3) <- 4; 
  b.(1).(4) <- -1; b.(1).(5) <- -1; b.(2).(3) <- -1; b.(2).(6) <- -1; b.(3).(2) <- -1; b.(3).(4) <- -1; b.(3).(5) <- -1; b.(3).(7) <- -1; b.(4).(1) <- -1; b.(4).(3) <- -1; b.(4).(6) <- -1; b.(4).(8) <- -1; b.(5).(1) <- -1; b.(5).(3) <- -1; b.(5).(6) <- -1; b.(5).(8) <- -1; b.(6).(2) <- -1; b.(6).(4) <- -1; b.(6).(5) <- -1; b.(6).(7) <- -1; b.(7).(3) <- -1; b.(7).(6) <- -1; b.(8).(4) <- -1; b.(8).(5) <- -1;
  b.(2).(2) <- -16; b.(2).(7) <- -16; b.(7).(2) <- -16; b.(7).(7) <- -16;
  b.(2).(4) <- -3; b.(2).(5) <- -3; b.(4).(2) <- -3; b.(4).(7) <- -3; b.(5).(2) <- -3; b.(5).(7) <- -3; b.(7).(4) <- -3; b.(7).(5) <- -3;
  b.(3).(3) <- 2; b.(3).(6) <- 2; b.(6).(6) <- 2; b.(6).(6) <- 2;
  b

exception List_length_mismatch
let rec penalty_adder ms valuations =
  match (ms, valuations) with
  | ([], []) -> []
  | ((i,j)::mrest, valu::valrest) -> (reference_board.(i).(j) + valu) :: (penalty_adder mrest valrest)
  | _ -> raise List_length_mismatch


let increase_my_valids4 next_boards color ms = (*3手先の盤面における敵の有効手数を評価関数に据え、minmax法で評価した際の最善手を返す*)
  let ocolor = opposite_color color in
  let boards_2turns_ahead = List.map (fun b -> list_next_boards b ocolor (valid_moves b ocolor)) next_boards in (*2手先の盤面のリスト。盤面配列の2重リストになっていることに注意*)
  let boards_3turns_ahead = List.map (fun blist -> List.map (fun b -> list_next_boards b color (valid_moves b color)) blist) boards_2turns_ahead in (*3手先の盤面のリスト。盤面配列の3重リスト*)
  let boards_4turns_ahead = List.map (fun blistlist -> List.map (fun blist -> List.map (fun b -> list_next_boards b ocolor (valid_moves b ocolor)) blist) blistlist) boards_3turns_ahead in (*4手先の盤面のリスト*)
  let valuation_of_valuable_mv = List.map (fun blistlistlist -> min_of_list (List.map (fun blistlist -> max_of_list (List.map (fun blist -> min_of_list (List.map (fun b -> count_valid_moves b color) blist)) blistlist)) blistlistlist)) boards_4turns_ahead in (*"3手先の自分(このplay関数)の手番で、敵の有効手がもっとも少なくなるように自分が打つ"という風に敵が想定している、という風に想定し、3手先の敵の有効手数を最小化する手を3手目としてうち、それを阻止するために敵はそれを最大化する手を2手目で打ち...*)
  let valuation_of_nextboards = penalty_adder ms valuation_of_valuable_mv in
  let best_mv_idx = idx_of_max valuation_of_nextboards in (*valuation_of_nextboardsのなかで評価値の最も高い値のインデックス*)
  let best_move = List.nth ms best_mv_idx in (*msのインデックスと、next_boardsのインデックスが対応しているため、このようにできる*)
  best_move
  
let rec corner_checker ms =
  match ms with
  |[] -> (0,0)
  |(i,j) :: mrest -> if (i,j) = (1,8) || (i,j) = (8,1) || (i,j) = (1,1) || (i,j) = (8,8) then (i,j) else corner_checker mrest

let viewpoint = ref 0 (*相手の初手に応じてデータベースにそった盤面の見方に変える*)


let move_interpreter move =
  match move with
  | Pass -> "PASS"
  | GiveUp -> "GIVEUP"
  | Mv (i,j) ->
    (match (!viewpoint) with
    | 0 -> 
      let ci = char_of_int (i + int_of_char 'a' - 1) in
      let cj = char_of_int (j + int_of_char '1' - 1) in
      let s  = Bytes.make 2 ' ' in
      let _  = ( Bytes.set s 0 ci; Bytes.set s 1 cj) in
      Bytes.to_string s
    | 1 -> (*viepoint = 1の時はy = xに関して対称*)
      let ci = char_of_int (j + int_of_char 'a' - 1) in
      let cj = char_of_int (i + int_of_char '1' - 1) in
      let s  = Bytes.make 2 ' ' in
      let _  = ( Bytes.set s 0 ci; Bytes.set s 1 cj) in
      Bytes.to_string s
    | 2 ->
      let ci = char_of_int ((9-i) + int_of_char 'a' - 1) in
      let cj = char_of_int ((9-j) + int_of_char '1' - 1) in
      let s  = Bytes.make 2 ' ' in
      let _  = ( Bytes.set s 0 ci; Bytes.set s 1 cj) in
      Bytes.to_string s
    | 3 ->
      let ci = char_of_int ((9-j) + int_of_char 'a' - 1) in
      let cj = char_of_int ((9-i) + int_of_char '1' - 1) in
      let s  = Bytes.make 2 ' ' in
      let _  = ( Bytes.set s 0 ci; Bytes.set s 1 cj) in
      Bytes.to_string s)

let rec hist_interpreter hist =
  match hist with
  | [] -> ""
  | PMove move :: rest -> (hist_interpreter rest)^(move_interpreter move)
  | OMove move :: rest -> (hist_interpreter rest)^(move_interpreter move)

let char_x_to_i next_x = (*アルファベットを指し手の列の数字に変換する*)
  (int_of_char next_x) - (int_of_char 'a') + 1

let char_y_to_j next_y =
  (int_of_char next_y) - (int_of_char '1') + 1

let x = ref 0
let y = ref 0

let viewpoint_backer (i,j) = (*序盤データベースの視点に合わせて生成された最善手を、もとの視点に戻す関数*)
  match (!viewpoint) with
  | 0 -> (i,j)
  | 1 -> (j,i)
  | 2 -> (9-i,9-j)
  | 3 -> (9-j,9-i)

let rec match_finder hist =
  let ic = open_in file in
  let interpreted_hist = hist_interpreter hist in (*histをすべて"c1d5b2..."のように変換*)
  let len = String.length interpreted_hist in (*変換後の文字列長を獲得*)
  for i=1 to 11833 do (*logbook.gamの行数*)
      let reference_line = input_line ic in
      let reference = String.sub reference_line 0 (len-2) in
      if "d3"^reference = interpreted_hist then
        let next_x = String.get reference_line (len-2) in
        let next_y = String.get reference_line (len-1) in
        x := char_x_to_i next_x;
        y := char_y_to_j next_y;
      else 
        ()
  done;
    let i = !x in
    let j = !y in
    let (true_i,true_j) = viewpoint_backer (i,j) in
    x := 0; y := 0;
    close_in ic;
    if (true_i,true_j) = (9,9) then
      (0,0)
    else
    (true_i,true_j)



let viewpoint_changer hist = 
  let [OMove (Mv (i,j))] = hist in
  match (i,j) with
  |(4,3) -> ()
  |(3,4) -> viewpoint := 1
  |(5,6) -> viewpoint := 2
  |(6,5) -> viewpoint := 3


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



let play board color hist =
  let ms = valid_moves board color in
  let next_boards =  list_next_boards board color ms in (*自分が一手打った後の盤面のリスト*)
  let (ic,jc) = corner_checker ms in
  let stone_num = count_stones board in
  print_board board;
  (*let (im,jm) = centralizer ms in (*中心4*4の盤面にまだ有効手があればその手が格納、なければ(0,0)が格納*)*)
    if ms = [] then
      Pass
    else if (ic,jc) <> (0,0) then 
      Mv (ic,jc)
    else if stone_num = 4 then
      Mv (4,3)
    else if stone_num = 5 then (*自分が後手の時、相手の一手目を見て序盤データベースに適した盤面の解釈をする*)
      let (ib,jb) = viewpoint_changer hist; match_finder hist in
      if (ib,jb) <> (0,0) then
        Mv (ib,jb)
      else 
        let (i,j) = increase_my_valids4 next_boards color ms in
        Mv (i,j)
    else if stone_num < 25 then
      let (ib,jb) = match_finder hist in
      if (ib,jb) <> (0,0) then
        Mv (ib,jb)
      else 
        let (i,j) = increase_my_valids4 next_boards color ms in
        Mv (i,j)
    else if stone_num > 52 then
      match init_surely_win board color with
      |Win (i,j) -> Mv (i,j)
      |Undecidable ->  let (i,j) = increase_my_valids4 next_boards color ms in Mv (i,j)
    else
    let (i,j) = increase_my_valids4 next_boards color ms in
    Mv (i,j)

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
