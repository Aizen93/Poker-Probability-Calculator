open Comb;;
open Proba;;

let generate_deck ()  =
  let rank = [As; Roi; Dame; Valet; Dix; Neuf; Huit; Sept; Six; Cinq; Quatre; Trois; Deux] in
    let color = [Pique; Trefle; Coeur; Carreau] in
      let rec aux rank color1 deck = match rank with
        | [] -> List.rev deck
        | h::t -> let rec aux' color1 deck = match color1 with
                  | [] -> aux t color deck
                  | h'::t' -> aux' t' (Card(h, h')::deck)
                  in aux' color1 deck
        in aux rank color []
;;

let give_player1_cards () =
  let deck = generate_deck () in
    let joueur1_card1 = (List.nth deck (Random.int 52)) in
      let joueur1_cards = (List.nth deck (Random.int 52)) in
        let rec aux card = if card = joueur1_card1 then aux (List.nth deck (Random.int 52))
                        else [joueur1_card1; card]
        in aux joueur1_cards
;;

let give_player2_cards joueur1_cards () =
  let deck = generate_deck () in
  let sort_deck = (List.filter (fun x -> not(List.mem x joueur1_cards)) deck) in
    let joueur2_card1 = (List.nth sort_deck (Random.int 50)) in
      let joueur2_cards = (List.nth sort_deck (Random.int 50)) in
        let rec aux card = if card = joueur2_card1 then aux (List.nth sort_deck (Random.int 50))
                        else [joueur2_card1; card]
        in aux joueur2_cards
;;
let generate_table j1_cards j2_cards () =
  let deck = generate_deck () in
    let sort_deck = (List.filter (fun x -> not(List.mem x (j1_cards@j2_cards))) deck) in
      let frst_card = (List.nth sort_deck (Random.int 48)) in
        let rec aux d table = match d with
              | [] -> []
              | h::t -> if List.length table = 5 then table
                        else let x = List.nth d (Random.int 48) in
                              if (List.mem x table) then aux d table
                              else aux d (x::table)
        in aux sort_deck [frst_card]
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

let print_card card = match card with
  | None -> print_string ("Aucune carte\n")
  | Card(a,b) -> print_string ("- "^(rang_to_string a)^" "^(color_to_string b)^"\n")
;;

let start_game () =
  let joueur1_cards = give_player1_cards ()
    in (print_string "Joueur1:\n"; print_card (List.hd joueur1_cards); print_card (List.nth joueur1_cards 1));

  let joueur2_cards = give_player2_cards joueur1_cards ()
    in (print_string "Joueur2:\n"; print_card (List.hd joueur2_cards); print_card (List.nth joueur2_cards 1));

  let table_cards = generate_table joueur1_cards joueur2_cards ()
    in (print_string "Table:\n"; print_card (List.hd table_cards); print_card (List.nth table_cards 1); print_card (List.nth table_cards 2));

  print_string "Probabilité de victoire:\n";
  let proba = proba_double (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, None, None)
    in (print_string "joueur1: "; print_float (fst proba); print_newline ();
      print_string "joueur2: "; print_float (snd proba); print_newline ());

  print_string "Player 1, would you like a ShowDown? ";
  let xx = read_line () in
    let rec respond1 s liste_table n = match s with
      | "y" | "yes" -> if n=3 then let proba = proba_double (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, None,None)
                          in (print_string "joueur1: "; print_float (fst proba); print_newline ();
                          print_string "joueur2: "; print_float (snd proba); print_newline ())
                        else if n=4 then let proba = proba_double (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, List.nth table_cards 3,None)
                          in (print_string "joueur1: "; print_float (fst proba); print_newline ();
                            print_string "joueur2: "; print_float (snd proba); print_newline ())
                        else print_int (compare_hands (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, List.nth table_cards 3,List.nth table_cards 4))
      | "n" | "no" -> ((print_string "Player 2, would you like a ShowDown? ");
                      let x = read_line () in
                        let rec respond2 ss = match ss with
                          | "yes" | "y" -> if n=3 then 
                                            let proba = proba_double (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, None,None)
                                              in (print_string "joueur1: "; print_float (fst proba); print_newline ();
                                              print_string "joueur2: "; print_float (snd proba); print_newline ())
                                            else if n=4 then 
                                              let proba = proba_double (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, List.nth table_cards 3,None)
                                                in (print_string "joueur1: "; print_float (fst proba); print_newline ();
                                                  print_string "joueur2: "; print_float (snd proba); print_newline ())
                                            else print_int (compare_hands (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, List.nth table_cards 3,List.nth table_cards 4))
                          | "no" | "n" -> ((print_string "Probabilité de victoire:\n");
                                          if n=4 then
                                            ((print_string "Table:\n"; print_card (List.hd table_cards); print_card (List.nth table_cards 1); print_card (List.nth table_cards 2), print_card (List.nth table_cards 3));
                                            let proba = proba_double (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, List.nth table_cards 2, None)
                                              in (print_string "joueur1: "; print_float (fst proba); print_newline ();
                                                print_string "joueur2: "; print_float (snd proba); print_newline ()))
                                          else if n=5 then
                                            ((print_string "Table:\n"; print_card (List.hd table_cards); print_card (List.nth table_cards 1); print_card (List.nth table_cards 2), print_card (List.nth table_cards 3), print_card (List.nth table_cards 4));
                                            let proba = proba_double (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, List.nth table_cards 2, List.nth table_cards 3)
                                              in (print_string "joueur1: "; print_float (fst proba); print_newline ();
                                                print_string "joueur2: "; print_float (snd proba); print_newline ()))
                                          else print_int (compare_hands (List.hd joueur1_cards, List.nth joueur1_cards 1) (List.hd joueur2_cards, List.nth joueur2_cards 1) (List.hd table_cards, List.nth table_cards 1, List.nth table_cards 2, List.nth table_cards 3,List.nth table_cards 4));
                                          respond1 "" ((List.nth table_cards n)::liste_table) (n+1))
                          | _ -> ((print_string "Player 2, would you like a ShowDown? "); respond2 (read_line ()))
                        in respond2 x)
      | _ -> ((print_string "Player 1, would you like a ShowDown? "); respond1 (read_line ()) liste_table n)
    in respond1 xx [List.hd table_cards; List.nth table_cards 1; List.nth table_cards 2; None; None] 4
;;

start_game ();;