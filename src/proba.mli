val generate_deck : unit -> Comb.carte list
val generate_possible_cards :
  Comb.donne ->
  Comb.donne ->
  Comb.carte list ->
  int ->
  (Comb.donne * Comb.donne *
   (Comb.carte * Comb.carte * Comb.carte * Comb.carte * Comb.carte))
  list
val get_last : 'a * 'b * 'c -> 'c
val get_midle : 'a * 'b * 'c -> 'b
val generate_possible_donne :
  Comb.donne ->
  Comb.table ->
  Comb.carte list ->
  int ->
  (Comb.donne * Comb.donne *
   (Comb.carte * Comb.carte * Comb.carte * Comb.carte * Comb.carte))
  list
val proba_double : Comb.donne -> Comb.donne -> Comb.table -> float * float
val map : ('a -> 'b) -> 'a list -> 'b list
val proba_simple : Comb.donne -> Comb.table -> float