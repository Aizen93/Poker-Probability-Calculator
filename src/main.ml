open Comb;;
open Proba;;
open Str;;

exception Wrong_rank_format;;
exception Wrong_color_format;;
exception Wrong_file_format;;

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

(* Fonction qui lit un fichier ligne par ligne et fait appel a la fonction parse afin d'en extraire les données et en fonction de cela renvoie les probabilités de victoire des joueurs.
type string -> float * float *)
let load_file file =
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
                    (-2., proba_simple don2 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4))
                  else if (fst don1) != None && (fst don2) = None then
                    (proba_simple don1 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4), -2.)
                  else if (fst don1) != None && (fst don2) != None then
                      if (List.nth tables 4) != None then let res = (compare_hands don1 don2 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4))
                        in if res = 1 then (1.,0.) else if res = -1 then (0.,1.) else (0., 0.)
                      else proba_double don1 don2 (List.hd tables, List.nth tables 1, List.nth tables 2, List.nth tables 3, List.nth tables 4)
                  else raise Wrong_file_format
        with End_of_file -> (-2., -2.)
      in aux ci (None, None) (None, None) (None, None, None, None, None) 0
;;

(* Fonction principale de l'exécutable compute qui affiche les résulats en fonction des informations présentes dans le fichier.
type unit -> unit *)
let main () =
  let file = Sys.argv.(1) in let resultat = load_file file in
    if (fst resultat) = 1. && (snd resultat) = 0. then print_string "Le joueur 1 est gagnant !\n"
    else if (fst resultat) = 0. && (snd resultat) = 1. then print_string "Le joueur 2 est gagnant !\n"
    else if (fst resultat) = 0. && (snd resultat) = 0. then print_string "La partie est nulle !\n"
    else if (fst resultat) = (-2.) then Printf.printf("Joueur 2: %f\n") (snd resultat)
    else if (snd resultat) = (-2.) then Printf.printf ("Joueur 1: %f\n") (fst resultat)
    else if (fst resultat) != (snd resultat) then Printf.printf ("Joueur 1: %f\nJoueur 2: %f\n") (fst resultat) (snd resultat)
    else print_string "Partie corrompue: STOP !"
;;

main ();;