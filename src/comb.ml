type color = Pique | Coeur | Carreau | Trefle;;

type rang = As | Roi | Dame | Valet | Dix | Neuf | Huit | Sept | Six| Cinq | Quatre | Trois | Deux;;

type carte =
  None
  | Card of rang * color;;

type donne = carte * carte;;

type table = carte * carte * carte * carte * carte;;

type comb =
   QuinteFlushRoyal of int
  | QuinteFlush of int
  | Carre of int * int
  | Full of int * int
  | Couleur of int * int * int * int * int
  | Suite of int
  | Brelan of int * int * int
  | DoublePaire of int * int * int
  | Paire of int * int * int * int
  | CarteHaute of int * int * int * int * int
;;

exception Liste_vide;;
exception First_k_empty;;
exception No_card;;

(* Fonction qui prend un rang et renvoie sa valeur en int avec AS = 1.
type rang -> int *)
let rang_to_int rank = match rank with
  | As -> 1
  | Roi -> 13
  | Dame -> 12
  | Valet -> 11
  | Dix -> 10
  | Neuf -> 9
  | Huit -> 8
  | Sept -> 7
  | Six -> 6
  | Cinq -> 5
  | Quatre -> 4
  | Trois -> 3
  | Deux -> 2
;;

(* Fonction qui prend un rang et renvoie sa valeur en int avec AS = 14.
type rang -> int *)
let rang_to_int2 rank = match rank with
  | As -> 14
  | Roi -> 13
  | Dame -> 12
  | Valet -> 11
  | Dix -> 10
  | Neuf -> 9
  | Huit -> 8
  | Sept -> 7
  | Six -> 6
  | Cinq -> 5
  | Quatre -> 4
  | Trois -> 3
  | Deux -> 2
;;

(* Fonction qui récupère le énième élément d'un quintuplet.
type a * 'a * 'a * 'a * 'a -> int -> 'a *)
let get_nth_element (a,b,c,d,e) n = if n = 0 then a
  else if n = 1 then b else if n = 2 then c else if n = 3 then d else e
;;

(* Fonction qui renvoie la liste des n premiers éléments d'une liste.
type int -> 'a list -> 'a list *)
let rec get_firstn n xs = match xs with
  | [] -> raise First_k_empty
  | x::xs -> if n=1 then [x] else x::get_firstn (n-1) xs
;;

(* Fonction qui renvoie le rang d'une carte.
type carte -> rang *)
let fst_card c = match c with
  | None -> raise No_card
  | Card (f,s) -> f
;;

(* Fonction qui renvoie la couleur d'une carte.
type carte -> color *)
let snd_card c = match c with
  | None -> raise No_card
  | Card (f,s) -> s
;;

(* Fonction qui renvoie une liste de cartes qui ont la meme couleur.
type carte list -> int -> carte list *)
let rec get_same_color_cards (list_cards: carte list) n = match list_cards with
  | [] -> raise Liste_vide
  | h'::t' -> let same_color_cards = (List.filter (fun (x: carte) -> (snd_card x) = (snd_card h')) list_cards) in
          if (List.length same_color_cards) < n then (get_same_color_cards t' n)
          else same_color_cards
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la quinte flush royale (As,Roi,Dame,Valet,Dix) si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isQuinteFlush_Royal (list_cards: carte list) =
  let same_color_cards =
    try (get_same_color_cards list_cards 5) with Liste_vide -> [] in
      let sorted_list_cards = (List.sort (fun x y -> if rang_to_int2 ((fst_card x)) < rang_to_int2 ((fst_card y)) then 1 else -1) same_color_cards) in
        if sorted_list_cards != [] then
          if (fst_card (List.hd sorted_list_cards)) = As && (fst_card (List.nth sorted_list_cards 1)) = Roi && (fst_card (List.nth sorted_list_cards 2)) = Dame && (fst_card (List.nth sorted_list_cards 3)) = Valet && (fst_card (List.nth sorted_list_cards 4)) = Dix  then
            let res = try (get_firstn 5 sorted_list_cards) with First_k_empty -> [] in res
          else []
        else []
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la plus forte quinte flush possible si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isQuinteFlush (list_cards: carte list) =
  let same_color_cards =
    try (get_same_color_cards list_cards 5) with Liste_vide -> [] in
    let sorted_list_cards = (List.sort (fun x y -> if (rang_to_int (fst_card x)) < (rang_to_int (fst_card y)) then 1 else -1) same_color_cards) in
      let rec aux sort_list = match sort_list with
        | [] -> []
        | h'::t' -> let rec aux' (sort_list: carte list) listQF rank = match sort_list with
                    | [] -> if ((List.length listQF) = 5) then (List.rev listQF) else aux t'
                    | h::t -> if (rang_to_int (fst_card h)) = rank then (aux' t (h::listQF) (rank-1))
                              else aux' t listQF rank
                    in aux' sorted_list_cards [] (rang_to_int (fst_card h'))
        in aux sorted_list_cards
;;

(* Trouve le premier element qui existe dans l1 mais pas dans l2 et le renvoie.
type 'a list -> 'a list -> 'a *)
let rec not_in l1 l2 = match l1 with
  | [] -> raise Liste_vide
  | h::t -> if (List.mem h l2) then (not_in t l2)
            else h
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant le plus fort carre possible si il existe et sinon une liste vide.
type carte list -> carte list *)
let rec isCarre (list_cards:carte list) = match list_cards with
  | [] -> []
  | h::t -> let same_rank_cards = (List.filter (fun x -> (fst_card x) = (fst_card h)) list_cards) in
    if (List.length same_rank_cards) < 4 then isCarre t
    else try
          List.rev ((not_in list_cards same_rank_cards)::same_rank_cards)
          with Liste_vide -> []
;;

(* Filtre les rang d'une combinaison de type Full et renvoie une liste de carte.
type int -> carte list -> carte list *)
let rec filtrer_rang n (sort_list: carte list) = match sort_list with
    | [] -> []
    | h::t -> let same_rank_cards = (List.filter (fun x -> (fst_card x) = (fst_card h)) sort_list) in
                if (List.length same_rank_cards) < n then filtrer_rang n t
                else try (get_firstn n same_rank_cards) with First_k_empty -> []
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant le plus fort full possible si il existe et sinon une liste vide.
type carte list -> carte list *)
let isFull (list_cards: carte list) =
  let sorted_list_cards = (List.sort (fun x y -> if (fst_card x) > (fst_card y) then 1 else -1) list_cards) in
    let three_rank_cards = (filtrer_rang 3 sorted_list_cards) in
      let two_rank_cards = (filtrer_rang 2 (List.filter (fun x -> not(List.mem x three_rank_cards)) sorted_list_cards)) in
        if (List.length (three_rank_cards@two_rank_cards)) = 5 then three_rank_cards@two_rank_cards
        else []
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la plus forte couleur possible si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isCouleur (list_cards: carte list) =
  let sorted_list_cards = (List.sort (fun x y -> if (fst_card x) > (fst_card y) then 1 else -1) list_cards) in
    try
    let same_color_cards = (get_same_color_cards sorted_list_cards 5) in  (get_firstn 5 same_color_cards)
    with
      | First_k_empty -> []
      | Liste_vide -> []
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la suite (5,4,3,2,1) si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isSuite_Royal (list_cards: carte list) =
  let suiteR = List.filter (fun x -> rang_to_int (fst_card x) <= 5) list_cards in
    let sorted_list_cards = (List.sort (fun x y -> if rang_to_int (fst_card x) < rang_to_int (fst_card y) then 1 else -1) suiteR) in
      if sorted_list_cards = [] then []
      else let res = let rec aux (sort_list: carte list) listSu rank = match sort_list with
                      | [] -> if ((List.length listSu) = 5) then (List.rev listSu) else []
                      | h::t -> if ((List.length listSu) = 5) then (List.rev listSu)
                                else if rang_to_int (fst_card h) = rank then aux t (h::listSu) (rank-1)
                                  else aux t listSu rank
                      in aux sorted_list_cards [] (rang_to_int (fst_card (List.hd sorted_list_cards)))
      in res
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la plus forte suite possible si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isSuite (list_cards: carte list) =
  let suiteR = isSuite_Royal list_cards in if suiteR != [] then suiteR
  else let sorted_list_cards = (List.sort (fun x y -> if ((fst_card x) > (fst_card y)) then 1 else -1) list_cards) in
    let rec aux sort_list = match sort_list with
      | [] -> []
      | h::t -> let rec aux' (sort_list: carte list) listSu rank = match sort_list with
                    | [] -> if ((List.length listSu) = 5) then (List.rev listSu) else aux t
                    | h'::t' -> if ((List.length listSu) = 5) then (List.rev listSu)
                                else if rang_to_int2 (fst_card h') = rank then aux' t' (h'::listSu) (rank-1)
                                  else aux' t' listSu rank
                    in aux' sorted_list_cards [] (rang_to_int2 (fst_card h))
      in aux sorted_list_cards
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant le plus fort brelan possible si il existe et sinon une liste vide.
type carte list -> carte list *)
let isBrelan (list_cards: carte list) =
  let sorted_list_cards = (List.sort (fun x y -> if (fst_card x) > (fst_card y) then 1 else -1) list_cards) in
    let three_rank_cards = (filtrer_rang 3 sorted_list_cards) in
      if (three_rank_cards != []) then
        let fourth_card =
          try
            (not_in sorted_list_cards three_rank_cards)::three_rank_cards
          with Liste_vide -> []
          in try
              List.rev ((not_in sorted_list_cards fourth_card)::fourth_card)
              with Liste_vide -> []
      else []
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la plus forte double paire possible si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isDoublePaire (list_cards: carte list) =
  let sorted_list_cards = (List.sort (fun x y -> if (fst_card x) > (fst_card y) then 1 else -1) list_cards) in
    let two_rank_cards1 = (filtrer_rang 2 sorted_list_cards) in
      let two_rank_cards = (filtrer_rang 2 (List.filter (fun x -> not(List.mem x two_rank_cards1)) sorted_list_cards)) in
        if (List.length (two_rank_cards1@two_rank_cards)) = 4
          then let resultat = List.rev (two_rank_cards1@two_rank_cards) in
                  try List.rev ((not_in sorted_list_cards resultat)::resultat) with Liste_vide -> []
        else []
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la plus forte paire possible si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isPaire (list_cards: carte list) =
  let sorted_list_cards = (List.sort (fun x y -> if (fst_card x) > (fst_card y) then 1 else -1) list_cards) in
    let two_rank_cards = (filtrer_rang 2 sorted_list_cards) in
      if (two_rank_cards != []) then
        let third_card =
          try
            (not_in sorted_list_cards two_rank_cards)::two_rank_cards
          with Liste_vide -> []
          in let fourth_card =
              try
              (not_in sorted_list_cards third_card)::third_card
              with Liste_vide -> []
              in try
                    (List.rev ((not_in sorted_list_cards fourth_card)::fourth_card))
                  with Liste_vide -> []
      else []
;;

(* Fonction qui prend une liste de cartes(donne+table) et renvoie 5 cartes formant la plus forte carte haute possible si elle existe et sinon une liste vide.
type carte list -> carte list *)
let isCarteHaute (list_cards: carte list) =
  let sorted_list_cards = (List.sort (fun x y -> if (fst_card x) > (fst_card y) then 1 else -1) list_cards) in
    let resultat = try (get_firstn 5 sorted_list_cards) with First_k_empty -> [] in resultat
;;

(* Fonction qui prend une donne et une table et qui génère la combinaison la plus forte possible.
type donne -> table -> comb *)
let compute_comb_max (don: donne) (tab: table) =
  let liste_don = [fst don; snd don] in
    let liste_table =  [get_nth_element tab 0; get_nth_element tab 1; get_nth_element tab 2; get_nth_element tab 3; get_nth_element tab 4] in
      let combinaison_max = (liste_don@liste_table) in
        let isQFR = isQuinteFlush_Royal (combinaison_max) in
          if isQFR != []
          then QuinteFlushRoyal(14)
          else let isQF = isQuinteFlush (combinaison_max) in
            if isQF != []
            then QuinteFlush(rang_to_int2 (fst_card (List.hd isQF)))
            else let isC = isCarre (combinaison_max) in
                if isC != []
                then Carre(rang_to_int2 (fst_card (List.hd isC)), rang_to_int2 (fst_card (List.nth isC 4)))
                else let isF = isFull (combinaison_max) in
                  if isF != []
                  then Full((rang_to_int2 (fst_card (List.hd isF)), rang_to_int2 (fst_card (List.nth isF 3))))
                  else let isCo = isCouleur (combinaison_max) in
                      if isCo != []
                      then Couleur(rang_to_int2 (fst_card (List.hd isCo)), rang_to_int2 (fst_card (List.nth isCo 1)),rang_to_int2 (fst_card (List.nth isCo 2)),rang_to_int2 (fst_card (List.nth isCo 3)),rang_to_int2 (fst_card (List.nth isCo 4)))
                      else let isS = isSuite (combinaison_max) in
                          if isS != []
                          then Suite(rang_to_int2 (fst_card (List.hd isS)))
                          else let isB = isBrelan (combinaison_max) in
                              if isB != []
                              then Brelan(rang_to_int2 (fst_card (List.hd isB)), rang_to_int2 (fst_card (List.nth isB 3)), rang_to_int2 (fst_card (List.nth isB 4)))
                              else let isDP = isDoublePaire (combinaison_max) in
                                  if isDP != []
                                  then DoublePaire(rang_to_int2 (fst_card (List.hd isDP)), rang_to_int2 (fst_card (List.nth isDP 2)), rang_to_int2 (fst_card (List.nth isDP 4)))
                                  else let isP = isPaire (combinaison_max) in
                                      if isP != []
                                      then Paire(rang_to_int2 (fst_card (List.hd isP)), rang_to_int2 (fst_card (List.nth isP 2)), rang_to_int2 (fst_card (List.nth isP 3)), rang_to_int2 (fst_card (List.nth isP 4)))
                                      else let isCH = isCarteHaute (combinaison_max) in
                                        if isCH != []
                                        then CarteHaute(rang_to_int2 (fst_card (List.hd isCH)), rang_to_int2 (fst_card (List.nth isCH 1)), rang_to_int2 (fst_card (List.nth isCH 2)), rang_to_int2 (fst_card (List.nth isCH 3)), rang_to_int2 (fst_card (List.nth isCH 4)))
                                        else CarteHaute(rang_to_int2 (fst_card (List.hd isCH)), rang_to_int2 (fst_card (List.nth isCH 1)), rang_to_int2 (fst_card (List.nth isCH 2)), rang_to_int2 (fst_card (List.nth isCH 3)), rang_to_int2 (fst_card (List.nth isCH 4)))
;;

(* Fonction qui associe un entier a une combinaison. Utilisée lors de la comparaison.
type comb -> int *)
let comb_to_int combi = match combi with
  | QuinteFlushRoyal(_) -> 10
  | QuinteFlush(_) -> 9
  | Carre(_,_) -> 8
  | Full(_,_) -> 7
  | Couleur(_,_,_,_,_) -> 6
  | Suite(_) -> 5
  | Brelan(_,_,_) -> 4
  | DoublePaire(_,_,_) -> 3
  | Paire(_,_,_,_) -> 2
  | CarteHaute(_,_,_,_,_) -> 1
;;


(* Fonction qui récupère le contenu d'une combinaison
type comb -> int -> int *)
let getComb combi n = match combi with
  | QuinteFlushRoyal(a) -> a
  | QuinteFlush(a) -> a
  | Carre(a,b) -> if n = 0 then a else b
  | Full(a,b) -> if n = 0 then a else b
  | Couleur(a,b,c,d,e) -> if n = 0 then a else if n = 1 then b else if n = 2 then c else if n = 3 then d else e
  | Suite(a) -> a
  | Brelan(a,b,c) -> if n = 0 then a else if n = 1 then b else c
  | DoublePaire(a,b,c) -> if n = 0 then a else if n = 1 then b else c
  | Paire(a,b,c,d) -> if n = 0 then a else if n = 1 then b else if n = 2 then c else d
  | CarteHaute(a,b,c,d,e) -> if n = 0 then a else if n = 1 then b else if n = 2 then c else if n = 3 then d else e
;;

(* Fonction qui prend deux combinaisons de poker et renvoie 1 si la combinaison 1 est plus forte, -1 si la combinaison 2 est plus forte et 0 lorsqu'elle sont égales.
type comb -> comb -> int *)
let compare_comb c1 c2 =
    if (comb_to_int (c1))  > (comb_to_int (c2)) then 1
    else if (comb_to_int (c1)) < (comb_to_int (c2)) then -1
    else let egalite int_combi = match int_combi with
          | 10 -> 0
          | 9 | 5 -> if (getComb c1 0) > (getComb c2 0) then 1 else if (getComb c1 0) < (getComb c2 0) then -1 else 0
          | 8 | 7 -> if (getComb c1 0) > (getComb c2 0) then 1 else if (getComb c1 0) < (getComb c2 0) then -1 else
                      if (getComb c1 1) > (getComb c2 1) then 1 else if (getComb c1 1) < (getComb c2 1) then -1
                      else 0
          | 4 | 3 -> if (getComb c1 0) > (getComb c2 0) then 1 else if (getComb c1 0) < (getComb c2 0) then -1 else
                      if (getComb c1 1) > (getComb c2 1) then 1 else if (getComb c1 1) < (getComb c2 1) then -1
                      else if (getComb c1 2) > (getComb c2 2) then 1 else if (getComb c1 2) < (getComb c2 2) then -1
                      else 0
          | 2 -> if (getComb c1 0) > (getComb c2 0) then 1 else if (getComb c1 0) < (getComb c2 0) then -1 else
                  if (getComb c1 1) > (getComb c2 1) then 1 else if (getComb c1 1) < (getComb c2 1) then -1
                  else if (getComb c1 2) > (getComb c2 2) then 1 else if (getComb c1 2) < (getComb c2 2) then -1
                  else if (getComb c1 3) > (getComb c2 3) then 1 else if (getComb c1 3) < (getComb c2 3) then -1
                  else 0
          | 6 | 1 -> if (getComb c1 0) > (getComb c2 0) then 1 else if (getComb c1 0) < (getComb c2 0) then -1 else
                    if (getComb c1 1) > (getComb c2 1) then 1 else if (getComb c1 1) < (getComb c2 1) then -1
                    else if (getComb c1 2) > (getComb c2 2) then 1 else if (getComb c1 2) < (getComb c2 2) then -1
                    else if (getComb c1 3) > (getComb c2 3) then 1 else if (getComb c1 3) < (getComb c2 3) then -1
                    else if (getComb c1 4) > (getComb c2 4) then 1 else if (getComb c1 4) < (getComb c2 4) then -1
                    else 0
          | _ -> raise (Failure "Type de carte non reconnu.")
          in egalite (comb_to_int c1)
;;

(* Fonction qui prend deux donnes et une table et renvoie 1 sur le joueur 1 gagne, -1 si le joueur 2 gagne et 0 lors d'une parle nulle.
type donne -> donne -> table -> int *)
let compare_hands don1 don2 table =
  let resultat = compare_comb (compute_comb_max don1 table) (compute_comb_max don2 table) in
    if resultat = 1 then 1 else if resultat = -1 then -1 else 0
;;