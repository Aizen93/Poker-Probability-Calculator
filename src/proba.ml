open Comb;;


(* Fonction qui génère un deck complet de 52 cartes.
type unit -> carte list *)
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

(* Fonction qui prend deux donnes et une table incomplète de 3 à 4 cartes et renvoie la liste de tous les triplets de la forme (donne1, donne2, tt) avec tt une table avec ses possible complètions.
type donne -> donne -> table -> carte list -> int -> (donne * donne * (carte * carte * carte * carte * carte)) list *)
let rec generate_possible_cards (don1: donne) (don2: donne) existcards n = match n with
  | 3 -> let deck_cards = (generate_deck ()) in
              let fstcard = List.nth existcards 4 in
                let sndcard = List.nth existcards 5 in
                  let trdcard = List.nth existcards 6 in
                    let frtcard = List.nth existcards 7 in
                      let aa = (List.filter (fun x -> not(List.mem x existcards)) deck_cards)
                        in let b = List.map (fun x -> (don1, don2, (fstcard, sndcard, trdcard, frtcard, x))) aa
                          in b
  | 2 -> let deck_cards = (generate_deck ()) in
          let fstcard = List.nth existcards 4 in
            let sndcard = List.nth existcards 5 in
              let trdcard = List.nth existcards 6 in
                let aa = (List.filter (fun x -> not(List.mem x existcards)) deck_cards)
                  in let bb = let rec aux (liste: carte list) tmp = match liste with
                                | [] -> tmp
                                | h::t -> aux t ((List.map (fun x -> (don1, don2, (fstcard, sndcard, trdcard, h, x))) t)@tmp)
                                in aux aa []
                      in bb
  | _ -> raise (failwith "Wrong number of cards on incomplete table, either <2 or >4" )
;;

let get_last (a,b,c) = c;;
let get_midle (a,b,c) = b;;

(* Fonction qui prend une donne et une table de 3 à 5 cartes et renvoie la liste de tous les triplets de la forme (donne, dd, tt) avec dd les possibles donnes du deuxième joueur et tt la table et ses possibles complétions.
type donne -> table -> carte list -> int -> (donne * donne * (carte * carte * carte * carte * carte)) list *)
let rec generate_possible_donne (don:donne) (tab: table) existcards n = match n with
  | 4 -> let deck_cards = (generate_deck ()) in
          let fstcard = List.nth existcards 2 in
            let sndcard = List.nth existcards 3 in
              let trdcard = List.nth existcards 4 in
                let frtcard = List.nth existcards 5 in
                  let fitcard = List.nth existcards 6 in
                  let aa = (List.filter (fun x -> not(List.mem x existcards)) deck_cards)
                    in let dd = let rec aux (liste: carte list) tmp = match liste with
                                  | [] -> tmp
                                  | h::t -> aux t ((List.map (fun x -> (h, x)) t)@tmp)
                                  in aux aa []
                      in let tt = List.map (fun x -> (don, x, (fstcard, sndcard, trdcard, frtcard, fitcard))) dd
                        in tt
  | 3 -> let deck_cards = (generate_deck ()) in
          let fstcard = List.nth existcards 2 in
            let sndcard = List.nth existcards 3 in
              let trdcard = List.nth existcards 4 in
                let frtcard = List.nth existcards 5 in
                let aa = (List.filter (fun x -> not(List.mem x existcards)) deck_cards)
                  in let dd = let rec aux (liste: carte list) tmp = match liste with
                                | [] -> tmp
                                | h::t -> aux t ((List.map (fun x -> (h, x)) t)@tmp)
                                in aux aa []
                    in let bb = let rec aux' liste tmp = match liste with
                                  | [] -> tmp
                                  | h'::t' -> aux' t' ((generate_possible_cards don h' [fst don; snd don; fst h'; snd h'; fstcard; sndcard; trdcard; frtcard] n)@tmp)
                      in aux' dd []
                    in bb
  | 2 -> let deck_cards = (generate_deck ()) in
          let fstcard = List.nth existcards 2 in
            let sndcard = List.nth existcards 3 in
              let trdcard = List.nth existcards 4 in
              let aa = (List.filter (fun x -> not(List.mem x existcards)) deck_cards)
                in let dd = let rec aux (liste: carte list) tmp = match liste with
                              | [] -> tmp
                              | h::t -> aux t ((List.map (fun x -> (h, x)) t)@tmp)
                              in aux aa []
                  in let tt = let rec aux' liste tmp = match liste with
                                | [] -> tmp
                                | h'::t' -> aux' t' ((generate_possible_cards don h' [fst don; snd don; fst h'; snd h'; fstcard; sndcard; trdcard] n)@tmp)
                                in aux' dd []
                      in tt
  | _ -> raise (failwith "Wrong number of cards on incomplete table, either <2 or >4" )
;;

(* Fonction qui prend deux donnes et une table contenant 3 a 5 cartes et renvoie la probabilité de victoire des deux joueurs.
type donne -> donne -> table -> float * float *)
let proba_double (don1: donne) (don2: donne) (tab: table) =
  let rec aux card liste n = match card with
    | None -> aux (get_nth_element tab (n-1)) liste (n-1)
    | Card(_,_) -> if (n = 4)
                    then let resultat = (compare_hands don1 don2 tab) in
                      if resultat = 1 then (float_of_int 1, float_of_int 0)
                      else if resultat = -1 then (float_of_int 0, float_of_int 1)
                      else (float_of_int 0, float_of_int 0)
                    else if (n = 3)
                        then let possible_cards = (generate_possible_cards don1 don2 [fst don1; snd don1; fst don2; snd don2; get_nth_element tab 0; get_nth_element tab 1; get_nth_element tab 2; get_nth_element tab 3] n)
                          in let count_win = List.map (fun x -> (compare_hands don1 don2 (get_last x))) possible_cards
                            in let size = float_of_int (List.length count_win)
                              in let joueur1 = float_of_int (List.length (List.filter (fun x -> x=1) count_win))
                                in let joueur2 = float_of_int (List.length (List.filter (fun x -> x=(-1)) count_win))
                                  in (joueur1/.size, joueur2/.size)
                    else if (n = 2)
                        then let possible_cards = (generate_possible_cards don1 don2 [fst don1; snd don1; fst don2; snd don2; get_nth_element tab 0; get_nth_element tab 1; get_nth_element tab 2] n)
                        in let count_win = List.map (fun x -> (compare_hands don1 don2 (get_last x))) possible_cards
                          in let size = float_of_int (List.length count_win)
                            in let joueur1 = float_of_int (List.length (List.filter (fun x -> x=1) count_win))
                              in let joueur2 = float_of_int (List.length (List.filter (fun x -> x=(-1)) count_win))
                                in (joueur1/.size, joueur2/.size)
                    else raise (failwith "Bad table format")
  in aux (get_nth_element tab 4) [] 4
;;

(* Fonction map recursive terminale.
type ('a -> 'b) -> 'a list -> 'b list *)
let map f l =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] l;;

(* Fonction qui prend une donne et une table contenant 3 a 5 cartes et renvoie la probabilité de victoire du joueur.
type donne -> table -> float *)
let proba_simple (don: donne) (tab: table) =
  let rec aux card n = match card with
    | None -> aux (get_nth_element tab (n-1)) (n-1)
    | Card(_,_) -> if (n = 4)
                    then let possible_donne = generate_possible_donne don tab [fst don; snd don; get_nth_element tab 0; get_nth_element tab 1; get_nth_element tab 2; get_nth_element tab 3; get_nth_element tab 4] n
                      in let count_win = map (fun x -> (compare_hands don (get_midle x) tab)) possible_donne
                        in let size = float_of_int (List.length count_win)
                          in let joueur = float_of_int (List.length (List.filter (fun x -> x=1) count_win))
                            in (joueur /. size)
                    else if (n = 3)
                      then let possible_donne = generate_possible_donne don tab [fst don; snd don; get_nth_element tab 0; get_nth_element tab 1; get_nth_element tab 2; get_nth_element tab 3] n
                        in let count_win = map (fun x -> (compare_hands don (get_midle x) (get_last x))) possible_donne
                          in let size = float_of_int (List.length count_win)
                            in let joueur = float_of_int (List.length (List.filter (fun x -> x=1) count_win))
                              in (joueur /. size)
                    else if (n = 2)
                      then let possible_donne = generate_possible_donne don tab [fst don; snd don; get_nth_element tab 0; get_nth_element tab 1; get_nth_element tab 2] n
                        in let count_win = map (fun x -> (compare_hands don (get_midle x) (get_last x))) possible_donne
                          in let size = float_of_int (List.length count_win)
                            in let joueur = float_of_int (List.length (List.filter (fun x -> x=1) count_win))
                              in (joueur /. size)
                    else -2.
  in aux (get_nth_element tab 4) 4
;;