open Comb;;
open Proba;;
open Str;;
open Graphics;;


exception Wrong_rank_format;;
exception Wrong_color_format;;
exception Wrong_file_format;;

type color = Graphics.color;;

let draw_on_screen s x y color =
  moveto x y;
  set_color(color);
  draw_string s;
;;

let xj1 = 130;;
let yj1 = 500;;
let jlargeur = 100;;
let jhauteur = 150;;
let rlargeur = 150;;
let rhauteur = 150;;

let xj2 = 130;;
let yj2 = 50;;
let gray = rgb 150 150 150;;

let draw_board () =
  set_color(black);
  fill_rect 100 250 600 200;
  set_color(white);
  moveto 220 450;
  lineto 220 250;
  moveto 340 450;
  lineto 340 250;
  moveto 460 450;
  lineto 460 250;
  moveto 580 450;
  lineto 580 250;

  set_color(gray);
  fill_rect xj1 yj1 jlargeur jhauteur;
  fill_rect (xj1+110) yj1 jlargeur jhauteur;
  fill_rect (xj1+360) yj1 rlargeur rhauteur;

  set_color(gray);
  fill_rect xj2 yj2 jlargeur jhauteur;
  fill_rect (xj2+110) yj2 jlargeur jhauteur;
  fill_rect (xj2+360) yj2 rlargeur rhauteur;
;;

(* Fonction qui prend un rang et renvoie sa valeur en string.
type rang -> string *)
let rang_to_string rank = match rank with
  | As -> "As"
  | Roi -> "Roi"
  | Dame -> "Dame"
  | Valet -> "Valet"
  | Dix -> "10"
  | Neuf -> "9"
  | Huit -> "8"
  | Sept -> "7"
  | Six -> "6"
  | Cinq -> "5"
  | Quatre -> "4"
  | Trois -> "3"
  | Deux -> "2"
;;

(* Fonction qui prend une couleur et renvoie sa valeur en string
type color -> string *)
let color_to_string couleur = match couleur with
  | Pique -> "Pique"
  | Coeur -> "Coeur"
  | Carreau -> "Carreau"
  | Trefle -> "Trefle"
;;

let ouvrir_fenetre () =
  open_graph(" 800x700");
  set_window_title("Calculatrice poker");
  draw_board ()
;;

(* Fonction qui prend une chaine de charactère et la parse afin d'obtenir la liste de cartes qu'elle représente.
type string -> carte list *)
let parse str =
  let res0 = Str.split (Str.regexp " ") str in
   let res = List.map (fun x -> Str.split (Str.regexp "") x) res0 in
    let rec aux res liste = match res with
      | [] -> if ((List.length liste) = 2 || (List.length liste) = 5) then (List.rev liste)
              else if (List.length liste) = 3 then List.rev (None::None::liste)
              else if (List.length liste) = 4 then List.rev (None::liste)
              else []
      | h'::t' -> let rec aux' h = match h with
                  | [] -> []
                  | h::t -> if h = "?" then aux t' (None::None::liste)
                            else let a =
                              if h = "A" then As
                              else if h = "R" then Roi
                              else if h = "D" then Dame
                              else if h = "V" then Valet
                              else if h = "1" then Dix
                              else if h = "9" then Neuf
                              else if h = "8" then Huit
                              else if h = "7" then Sept
                              else if h = "6" then Six
                              else if h = "5" then Cinq
                              else if h = "4" then Quatre
                              else if h = "3" then Trois
                              else if h = "2" then Deux
                              else raise Wrong_rank_format
                                in let b = List.fold_left (fun x y -> if x = "0" then y else x^y) "" t in
                                  if b = "co" then aux t' (Card (a,Coeur)::liste)
                                  else if b = "ca" then  aux t' (Card (a,Carreau)::liste)
                                  else if b = "t" then  aux t' (Card (a,Trefle)::liste)
                                  else if b = "p" then  aux t' (Card (a,Pique)::liste)
                                  else raise Wrong_color_format
                  in aux' h'
      in aux res []
;;

let load_file2 file =
    let ci = open_in file in
      let rec aux ci (don1: donne) (don2: donne) (table: table) n =
        try
          let x = input_line ci in
            if(n = 0) then let (donn1: carte list) = try parse x with
              | Wrong_rank_format -> []
              | Wrong_color_format -> []
              in (aux ci (List.nth donn1 0, List.nth donn1 1) don2 table (n+1))
            else if (n = 1)
              then let donn2 = try parse x with
              | Wrong_rank_format -> []
              | Wrong_color_format -> []
              in  (aux ci don1 (List.nth donn2 0, List.nth donn2 1) table (n+1))
            else let tables = try parse x with
              | Wrong_rank_format -> []
              | Wrong_color_format -> []
              in if (fst don1) = None && (fst don2) != None then
                    ((None,None), don2, tables, -2., proba_simple don2 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4))
                  else if (fst don1) != None && (fst don2) = None then
                    (don1, (None,None), tables, proba_simple don1 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4), -2.)
                  else if (fst don1) != None && (fst don2) != None then
                      if (List.nth tables 4) != None then let res = (compare_hands don1 don2 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4))
                        in if res = 1 then
                            (don1, don2, tables, float_of_int res, 0.)
                          else if res = -1 then (don1, don2, tables, 0., -1.*.float_of_int res) else (don1, don2, tables, 0., 0.)
                      else let x = proba_double don1 don2 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4) in
                            (don1, don2 ,tables, fst x, snd x)
                  else raise Wrong_file_format
        with End_of_file -> ((None,None), (None,None), [None; None; None; None; None], -2., -2.)
      in aux ci (None, None) (None, None) (None, None, None, None, None) 0
;;

let get_float ((a: donne),(b: donne),(c: carte list),(d: float),(e: float)) n = if n = 3 then d else e
;;
let get_donne ((a: donne),(b: donne),(c: carte list),(d: float),(e: float)) n = if n = 0 then a else b
;;
let get_table ((a: donne),(b: donne),(c: carte list),(d: float),(e: float)) = c
;;

let draw_result resultat =
  clear_graph ();
  draw_board ();
  draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 0))))^" "^(color_to_string (snd_card (fst (get_donne resultat 0))))) (xj1) (yj1+75) black;
  draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 0))))^" "^(color_to_string (snd_card (snd (get_donne resultat 0))))) (xj1+110) (yj1+75) black;
  draw_on_screen ((string_of_float ((get_float resultat 3)*.100.))^"%") (xj1+420) (yj1+75) black;
  draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 1))))^" "^(color_to_string (snd_card (fst (get_donne resultat 1))))) (xj2) (yj2+75) black;
  draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 1))))^" "^(color_to_string (snd_card (snd (get_donne resultat 1))))) (xj2+110) (yj2+75) black;
  draw_on_screen ((string_of_float ((get_float resultat 4)*.100.))^"%") (xj2+420) (yj2+75) black;
  draw_on_screen ((rang_to_string (fst_card (List.hd (get_table resultat))))^" "^(color_to_string (snd_card (List.hd (get_table resultat))))) 110 350 white;
  draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 1)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 1)))) 230 350 white;
  draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 2)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 2)))) 350 350 white;
  draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 3)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 3)))) 470 350 white;
  draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 4)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 4)))) 590 350 white
  ;;

let draw_result_unfinished resultat nbr_j nbr_tab = match nbr_j with
  | 2 -> if nbr_tab = 4 then
          (clear_graph ();
          draw_board ();
          print_string "Cas avec 2 joueur et 4 cartes sur table\n";
          draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 0))))^" "^(color_to_string (snd_card (fst (get_donne resultat 0))))) (xj1) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 0))))^" "^(color_to_string (snd_card (snd (get_donne resultat 0))))) (xj1+110) (yj1+75) black;
          draw_on_screen ((string_of_float ((get_float resultat 3)*.100.))^"%") (xj1+420) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 1))))^" "^(color_to_string (snd_card (fst (get_donne resultat 1))))) (xj2) (yj2+75) black;
          draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 1))))^" "^(color_to_string (snd_card (snd (get_donne resultat 1))))) (xj2+110) (yj2+75) black;
          draw_on_screen ((string_of_float ((get_float resultat 4)*.100.))^"%") (xj2+380) (yj2+75) black;
          draw_on_screen ((rang_to_string (fst_card (List.hd (get_table resultat))))^" "^(color_to_string (snd_card (List.hd (get_table resultat))))) 110 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 1)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 1)))) 230 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 2)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 2)))) 350 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 3)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 3)))) 470 350 white)
        else if nbr_tab = 3 then
          (clear_graph ();
          draw_board ();
          print_string "Cas avec 2 joueur et 3 cartes sur table\n";
          draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 0))))^" "^(color_to_string (snd_card (fst (get_donne resultat 0))))) (xj1) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 0))))^" "^(color_to_string (snd_card (snd (get_donne resultat 0))))) (xj1+110) (yj1+75) black;
          draw_on_screen ((string_of_float ((get_float resultat 3)*.100.))^"%") (xj1+380) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 1))))^" "^(color_to_string (snd_card (fst (get_donne resultat 1))))) (xj2) (yj2+75) black;
          draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 1))))^" "^(color_to_string (snd_card (snd (get_donne resultat 1))))) (xj2+110) (yj2+75) black;
          draw_on_screen ((string_of_float ((get_float resultat 4)*.100.))^"%") (xj2+380) (yj2+75) black;
          draw_on_screen ((rang_to_string (fst_card (List.hd (get_table resultat))))^" "^(color_to_string (snd_card (List.hd (get_table resultat))))) 110 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 1)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 1)))) 230 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 2)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 2)))) 350 350 white)
        else raise Wrong_file_format
  | 1 -> if nbr_tab = 5 then
          (clear_graph ();
          draw_board ();
          print_string "Cas avec 1 joueur et 5 cartes sur table\n";
          draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 0))))^" "^(color_to_string (snd_card (fst (get_donne resultat 0))))) (xj1) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 0))))^" "^(color_to_string (snd_card (snd (get_donne resultat 0))))) (xj1+110) (yj1+75) black;
          draw_on_screen ((string_of_float ((get_float resultat 3)*.100.))^"%") (xj1+380) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (List.hd (get_table resultat))))^" "^(color_to_string (snd_card (List.hd (get_table resultat))))) 110 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 1)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 1)))) 230 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 2)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 2)))) 350 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 3)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 3)))) 470 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 4)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 4)))) 590 350 white)
        else if nbr_tab = 4 then
          (clear_graph ();
          draw_board ();
          print_string "Cas avec 1 joueur et 4 cartes sur table\n";
          draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 0))))^" "^(color_to_string (snd_card (fst (get_donne resultat 0))))) (xj1) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 0))))^" "^(color_to_string (snd_card (snd (get_donne resultat 0))))) (xj1+110) (yj1+75) black;
          draw_on_screen ((string_of_float ((get_float resultat 3)*.100.))^"%") (xj1+380) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (List.hd (get_table resultat))))^" "^(color_to_string (snd_card (List.hd (get_table resultat))))) 110 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 1)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 1)))) 230 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 2)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 2)))) 350 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 3)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 3)))) 470 350 white)
        else if nbr_tab = 3 then
          (clear_graph ();
          draw_board ();
          print_string "Cas avec 1 joueur et 3 cartes sur table\n";
          draw_on_screen ((rang_to_string (fst_card (fst (get_donne resultat 0))))^" "^(color_to_string (snd_card (fst (get_donne resultat 0))))) (xj1) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (snd (get_donne resultat 0))))^" "^(color_to_string (snd_card (snd (get_donne resultat 0))))) (xj1+110) (yj1+75) black;
          draw_on_screen ((string_of_float ((get_float resultat 3)*.100.))^"%") (xj1+380) (yj1+75) black;
          draw_on_screen ((rang_to_string (fst_card (List.hd (get_table resultat))))^" "^(color_to_string (snd_card (List.hd (get_table resultat))))) 110 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 1)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 1)))) 230 350 white;
          draw_on_screen ((rang_to_string (fst_card (List.nth (get_table resultat) 2)))^" "^(color_to_string (snd_card (List.nth (get_table resultat) 2)))) 350 350 white)
        else raise Wrong_file_format
  | _ -> raise Wrong_file_format
;;

let read_files file =
  let resultat = load_file2 file in
    if ((fst (get_donne resultat 0)) != None && (fst (get_donne resultat 1)) != None)
      then
        if ((get_float resultat 3) = 1. && (get_float resultat 4) = 0.)
          then draw_result resultat
        else if ((get_float resultat 3) = 0. && (get_float resultat 4) = 1.)
          then draw_result resultat
        else if ((get_float resultat 3) = 0. && (get_float resultat 4) = 0.)
          then draw_result resultat
        else
            if ((List.nth (get_table resultat) 3) != None) then draw_result_unfinished resultat 2 4
            else draw_result_unfinished resultat 2 3
    else
      if ((List.nth (get_table resultat) 4) != None) then (draw_result_unfinished resultat 1 5)
      else if ((List.nth (get_table resultat) 3) != None) then (draw_result_unfinished resultat 1 4)
      else (draw_result_unfinished resultat 1 3)
;;

let main () =
    let file = Sys.argv.(1) in read_files file
;;

let rec loop () =
  let e = wait_next_event [Key_pressed] in
  if e.key <> 'q' then loop () else ()
;;

let () =
  try
    ouvrir_fenetre ();
    main ();
    loop ();
  with
  | Graphic_failure("fatal I/O error") -> print_string "Thank you, bye !"; print_newline ();
;;
