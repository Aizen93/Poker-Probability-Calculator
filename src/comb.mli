type color = Pique | Coeur | Carreau | Trefle
type rang =
    As
  | Roi
  | Dame
  | Valet
  | Dix
  | Neuf
  | Huit
  | Sept
  | Six
  | Cinq
  | Quatre
  | Trois
  | Deux
type carte = None | Card of rang * color
type donne = carte * carte
type table = carte * carte * carte * carte * carte
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

exception Liste_vide
exception First_k_empty
exception No_card

val rang_to_int : rang -> int
val rang_to_int2 : rang -> int
val get_nth_element : 'a * 'a * 'a * 'a * 'a -> int -> 'a
val get_firstn : int -> 'a list -> 'a list
val fst_card : carte -> rang
val snd_card : carte -> color
val get_same_color_cards : carte list -> int -> carte list
val isQuinteFlush_Royal : carte list -> carte list
val isQuinteFlush : carte list -> carte list
val not_in : 'a list -> 'a list -> 'a
val isCarre : carte list -> carte list
val filtrer_rang : int -> carte list -> carte list
val isFull : carte list -> carte list
val isCouleur : carte list -> carte list
val isSuite_Royal : carte list -> carte list
val isSuite : carte list -> carte list
val isBrelan : carte list -> carte list
val isDoublePaire : carte list -> carte list
val isPaire : carte list -> carte list
val isCarteHaute : carte list -> carte list
val compute_comb_max : donne -> table -> comb
val comb_to_int : comb -> int
val getComb : comb -> int -> int
val compare_comb : comb -> comb -> int
val compare_hands : donne -> donne -> table -> int