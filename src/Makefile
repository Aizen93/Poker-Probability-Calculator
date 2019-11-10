all : comb.cmx proba.cmx main.cmx interface.cmx game.cmx
	ocamlopt str.cmxa comb.cmx proba.cmx main.cmx -o compute
	ocamlopt str.cmxa graphics.cmxa comb.cmx proba.cmx interface.cmx -o gui
	ocamlopt comb.cmx proba.cmx game.cmx -o cli

comb.cmx : comb.ml comb.cmi
	ocamlopt -c comb.ml

comb.cmi : comb.mli
	ocamlopt comb.mli

proba.cmx : proba.ml proba.cmi
	ocamlopt -c proba.ml

proba.cmi : proba.mli
	ocamlopt proba.mli

main.cmx : main.ml proba.cmi comb.cmi
	ocamlopt -c str.cmxa main.ml

interface.cmx : interface.ml proba.cmi comb.cmi
	ocamlopt -c str.cmxa graphics.cmxa interface.ml

game.cmx : game.ml proba.cmi comb.cmi
	ocamlopt -c game.ml

clean :
	rm *.cmo *.cmi *.o *cmx

mrproper :
	rm compute gui cli