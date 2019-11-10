open Calc;;
open Main;;

let rec give_player_cards (in_game_cards: carte list) index = match in_game_cards with
  | [] -> raise Liste_vide
  | h::t -> if index = 0 then h
    else give_player_cards t (index-1)
;;


let isSuite_Royal (list_cards: carte list) =
  let suiteR = List.filter (fun x -> rang_to_int (fst_card x) <= 5) list_cards in
  if (List.length suiteR) = 5 then (List.sort (fun x y -> if rang_to_int (fst_card x) < rang_to_int (fst_card y) then 1 else -1) suiteR) else []
;;

let isSuite (list_cards: carte list) =
let suiteR = isSuite_Royal list_cards in if suiteR != [] then suiteR
else let sorted_list_cards = (List.sort (fun x y -> if ((fst_card x) > (fst_card y)) then 1 else -1) list_cards) in
  let rec aux sort_list = match sort_list with
    | [] -> []
    | h'::t' -> let rec aux' (sort_list: carte list) listQF rank = match sort_list with
                | [] -> if ((List.length listQF) = 5) then (List.rev listQF) else aux t'
                | h::t -> if ((List.length listQF) = 5) then (List.rev listQF)
                          else if (rang_to_int (fst_card h)) = rank then
                            if (rang_to_int (fst_card h) = 1) then (aux' t (h::listQF) 13)
                            else (aux' t (h::listQF) (rank-1))
                          else aux' t listQF rank
                in aux' sorted_list_cards [] (rang_to_int (fst_card h'))
    in aux sorted_list_cards
;;

let max_liste l = match l with
  | [] -> failwith "None"
  | h::t ->  let rec helper (vu,reste) =  match reste with
              | [] -> vu
              | h'::t' -> let vu' = if h' > vu then h' else vu in
                          let reste' = t'
              in helper (vu',reste')
            in helper (h,t)
;;

let read_files file = 
  let resultat = load_file2 file in
    if (get_float resultat 3) = 1. && (get_float resultat 4) = 0. 
      then draw_result resultat
    else if (get_float resultat 3) = 0. && (get_float resultat 4) = 1.
      then draw_result resultat
    else if (get_float resultat 3) = 0. && (get_float resultat 4) = 0.
      then draw_result resultat
    else if (get_float resultat 3) != (-2.) && (get_float resultat 4) !=  (-2.) 
      then 
        if (List.nth (get_table resultat) 3) != None then draw_result_unfinished resultat 2 4
        else draw_result_unfinished resultat 2 3
    else if (get_float resultat 3) != (-2.) || (get_float resultat 4) != (-2.) 
      then 
        if (List.nth (get_table resultat) 4) != None then draw_result_unfinished resultat 1 5
        else if (List.nth (get_table resultat) 3) != None then draw_result_unfinished resultat 1 4
        else draw_result_unfinished resultat 1 3
    else
      (clear_graph ();
      draw_board (); 
      draw_on_screen "Sa maaarche" (xj2+110) (yj2+75) black)
;;

let (in_game_cards: carte list) = [Card (Dix, Pique); Card (Valet, Pique); Card (Roi, Carreau); Card (Dame, Trefle); Card (Valet, Coeur); Card (Dame, Pique); Card (Neuf, Pique); Card (Roi, Pique); Card (As, Pique)];;
let in_game_cards = [(Dix, Carreau); (Valet, Pique); (Roi, Carreau); (Dame, Pique); (Valet, Coeur); (Dame, Pique); (Sept, Pique); (Roi, Pique); (As, Pique)];;
let in_game_cards = [(As, Coeur);(Quatre, Pique);(Roi, Coeur);(As, Trefle);(Deux, Trefle);(Trois, Carreau);(Dame, Trefle);(Valet, Coeur);(Six, Pique)];;

let (in_game_cards: carte list) = [(Roi, Coeur); (Deux, Pique); (Roi, Pique); (Trois, Trefle); (Roi, Trefle); (Valet, Carreau); (Huit, Trefle); (Sept, Coeur); (Six, Pique)];;


let (player1_cards: donne) = (give_player_cards in_game_cards 0, give_player_cards in_game_cards 1) ;;
let (player2_cards:donne) = (give_player_cards in_game_cards 2, give_player_cards in_game_cards 3);;
let (table_cards: table) = (give_player_cards in_game_cards 4, give_player_cards in_game_cards 5, give_player_cards in_game_cards 6, give_player_cards in_game_cards 7, give_player_cards in_game_cards 8);;

let (list_cards: carte list) = ([fst player1_cards; snd player1_cards]@[get_nth_element table_cards 0; get_nth_element table_cards 1; get_nth_element table_cards 2; get_nth_element table_cards 3; get_nth_element table_cards 4]);;

compare_hands (Card(Neuf,Carreau), Card(Quatre,Pique)) (Card(As,Carreau), Card(Trois,Pique)) (Card(Roi,Carreau), Card(Dix,Carreau), Card(Valet,Carreau), Card(Dame, Carreau), Card(Deux, Pique));;

compare_comb (QuinteFlush (13)) (QuinteFlushRoyal (14));;

compute_comb_max (Card(Neuf,Carreau), Card(Quatre,Pique)) (Card(Roi,Carreau), Card(Dix,Carreau), Card(Valet,Carreau), Card(Dame, Carreau), Card(Deux, Pique));;
compute_comb_max (Card(As,Carreau), Card(Trois,Pique)) (Card(Roi,Carreau), Card(Dix,Carreau), Card(Valet,Carreau), Card(Dame, Carreau), Card(Deux, Pique));;

isQuinteFlush_Royal [Card(As,Carreau); Card(Trois,Pique); Card(Roi,Carreau); Card(Dix,Carreau); Card(Valet,Carreau); Card(Dame, Carreau); Card(Deux, Pique)];;
let b = proba_simple (Card(Trois, Coeur),Card(Trois, Pique))
(Card(Roi, Trefle),Card(Roi, Coeur),Card(Valet, Trefle),Card(Deux, Coeur),Card(Six, Pique));;

isDoublePaire [Card(Trois, Coeur);Card(Trois, Pique); Card(Roi, Trefle);Card(Roi, Coeur);Card(Trois, Trefle);Card(Deux, Coeur);Card(Six, Pique)]

let b = generate_possible_donne (Card(As,Pique),Card(Roi,Coeur))
(Card(Dix,Pique),Card(Neuf,Coeur),Card(Six,Coeur),Card(Sept,Carreau),None)
[Card(As,Pique);Card(Roi,Coeur);Card(Dix,Pique);Card(Neuf,Coeur);Card(Six,Coeur);Card(Sept,Carreau)] 4

let b21 = proba_simple (Card(Trois, Coeur),Card(Dix, Pique))
(Card(As, Trefle),Card(As, Carreau),Card(Trois, Carreau),Card(Deux, Pique),Card(Valet, Pique));;

isCarre [Card(Trois, Coeur);Card(Dix, Pique);Card(As, Trefle);Card(As, Carreau);Card(Trois, Carreau);Card(Deux, Pique);Card(Valet, Pique)]

let b22 = generate_possible_donne (Card(Roi, Coeur),Card(Deux, Pique))
(Card(Roi, Trefle),Card(Valet, Carreau),Card(Huit, Trefle),Card(Sept, Coeur),None);;


generate_possible_cards (Card(As, Pique), Card(As, Coeur)) (Card(Valet,Trefle), Card(Valet,Carreau))
[Card(As, Pique); Card(As, Coeur); Card(Valet,Trefle); Card(Valet,Carreau); Card(Dix,Pique);Card(Neuf,Coeur);Card(Six,Coeur);None;None] 2

let b23 = proba_simple (Card(As, Coeur),Card(As, Pique))
(Card(Deux, Trefle),Card(Trois, Carreau),Card(Roi, Trefle),Card(Cinq, Coeur),None);;

let b26 = proba_simple (Card(As, Coeur),Card(As, Pique))
(Card(Deux, Trefle),Card(Trois, Carreau),Card(Roi, Trefle),None,None);;

let b21 = generate_possible_donne (Card(Trois,Coeur),Card(Dix,Pique))
(Card(As,Trefle),Card(As,Carreau),Card(Trois,Carreau),Card(Deux,Pique),Card(Valet, Pique))
[Card(Trois,Coeur);Card(Dix,Pique);Card(As,Trefle);Card(As,Carreau);Card(Trois,Carreau);Card(Deux,Pique);Card(Valet, Pique)] 4


QF OK
compute_comb_max (Card(Neuf,Carreau), Card(Deux,Carreau)) (Card(Huit,Carreau), Card(Sept,Carreau), Card(Valet,Coeur), Card(Six, Carreau), Card(Cinq, Carreau));;

CArre d'AS OK
compute_comb_max (Card(As,Coeur), Card(As,Pique)) (Card(Huit,Carreau), Card(As,Carreau), Card(Valet,Coeur), Card(As, Trefle), Card(Cinq, Carreau));;
CArre non As OK
compute_comb_max (Card(Quatre,Coeur), Card(Quatre,Pique)) (Card(Huit,Carreau), Card(Quatre,Carreau), Card(Valet,Coeur), Card(Quatre, Trefle), Card(Cinq, Carreau));;

Full d'As OK
compute_comb_max (Card(As,Coeur), Card(As,Pique)) (Card(Deux,Carreau), Card(As,Carreau), Card(Valet,Coeur), Card(Quatre, Trefle), Card(Deux, Coeur));;
Full non As OK
compute_comb_max (Card(Deux,Coeur), Card(Deux,Pique)) (Card(Deux,Carreau), Card(Neuf,Carreau), Card(Neuf,Coeur), Card(Neuf, Trefle), Card(Cinq, Coeur));;

Couleur d'As OK
compute_comb_max (Card(Cinq,Coeur), Card(Quatre,Coeur)) (Card(Deux,Coeur), Card(Trois,Trefle), Card(Neuf,Coeur), Card(As, Coeur), Card(Cinq, Trefle));;

Suite d'As
compute_comb_max (Card(Cinq,Pique), Card(Quatre,Carreau)) (Card(Deux,Coeur), Card(Trois,Pique), Card(Neuf,Trefle), Card(As, Coeur), Card(Valet, Trefle));;
Suite non As
compute_comb_max (Card(Cinq,Pique), Card(Quatre,Carreau)) (Card(Deux,Coeur), Card(Trois,Pique), Card(Six,Trefle), Card(As, Coeur), Card(Valet, Trefle));;
isSuite [Card(Roi,Pique); Card(Dame,Carreau); Card(Deux,Coeur); Card(Trois,Pique); Card(Dix,Trefle); Card(As, Coeur); Card(Valet, Trefle)];;
isSuite [Card(Dix,Pique); Card(Sept,Carreau); Card(Quatre,Coeur); Card(Trois,Pique); Card(Neuf,Trefle); Card(Huit, Coeur); Card(Valet, Trefle)];;
isSuite [Card(Trois,Coeur); Card(Dix,Pique); Card(As,Trefle); Card(As,Carreau); Card(Trois,Carreau); Card(Deux, Pique); Card(Valet, Pique)];;
isSuite [Card(Cinq,Pique); Card(Quatre,Carreau); Card(Deux,Coeur); Card(Trois,Pique); Card(Neuf,Trefle); Card(As, Coeur); Card(Valet, Trefle)];;
isSuite [Card(Six,Pique); Card(Neuf,Carreau); Card(Cinq,Coeur); Card(Huit,Pique); Card(Neuf,Trefle); Card(Sept, Coeur); Card(Valet, Trefle)];;
Belan d'As
compute_comb_max (Card(Trois,Pique), Card(Trois,Carreau)) (Card(As,Coeur), Card(Huit,Carreau), Card(Six,Trefle), Card(Trois, Coeur), Card(Dame, Trefle));;

DP
compute_comb_max (Card(Roi,Pique), Card(Dame,Carreau)) (Card(Deux,Coeur), Card(Deux,Carreau), Card(Six,Trefle), Card(Trois, Coeur), Card(Valet, Trefle));;
Pair
compute_comb_max (Card(Roi,Coeur), Card(Deux,Pique)) (Card(Roi,Trefle), Card(Valet,Carreau), Card(Huit,Trefle), Card(Sept, Coeur), Card(Six, Pique));;

let possible_donne = (generate_possible_donne (Card(Trois,Coeur), Card(Dix,Pique))
(Card(As,Trefle), Card(As,Carreau), Card(Trois,Carreau), Card(Deux, Pique), Card(Valet, Pique))
[Card(Trois,Coeur); Card(Dix,Pique); Card(As,Trefle); Card(As,Carreau); Card(Trois,Carreau); Card(Deux, Pique); Card(Valet, Pique)] 4);;

let test = map (fun x -> (compare_hands (Card(Trois,Coeur), Card(Dix,Pique))
(get_midle x)
(Card(As,Trefle), Card(As,Carreau), Card(Trois,Carreau), Card(Deux, Pique), Card(Valet, Pique))))
possible_donne;;
let egal = (List.filter (fun x-> (compare_comb (fst x) (snd x))=0 ) test);;
let win =  (List.filter (fun x-> (compare_comb (fst x) (snd x))=1) test);;
let loose =  (List.filter (fun x-> (compare_comb (fst x) (snd x))=(-1)) test);;



(* charge une image quelconque (.jpg,.png... comme supporté par
   camlimages) vers une matrice de triplets (r,g,b) d'entiers :
   (int*int*int)*array*array *)
   let load_rgb_matrix name =
    let img = Images.load name [] in
    let gimg = Graphic_image.array_of_image img in
    let rgb color =
      let quot n = n mod 256, n / 256 in
      let b, rg = quot color in
      let g, r = quot rg in
      r, g, b
    in
    Array.map (Array.map rgb) gimg
   ;;
  (* transforme une matrice de triplets (r,g,b) en une "image graphics"
     de type Graphics.image *)
  let to_graphics rgb_matrix =
    Graphics.make_image
      (Array.map
         (Array.map
            (fun (r, g, b) -> Graphics.rgb r g b))
         rgb_matrix)
   ;;
   
  let map_matrix f matrix = Array.map (Array.map f) matrix;;
   
  let invert_colors = map_matrix
    (fun (r, g, b) -> (255-r, 255-g, 255-b))
   ;;
   
  let () =
    (* charge l'image donnée en argument : "./test truc.png" *)
    let test = load_rgb_matrix Sys.argv.(1) in
    Graphics.open_graph " 800x600";
    (* dessine l'image une première fois *)
    Graphics.draw_image (to_graphics test) 0 0;
    ignore (Graphics.read_key ());
    (* dessine l'image avec les couleurs inversées *)
    Graphics.draw_image (to_graphics (invert_colors test)) 0 0;
    ignore (Graphics.read_key ());
    Graphics.close_graph ();;