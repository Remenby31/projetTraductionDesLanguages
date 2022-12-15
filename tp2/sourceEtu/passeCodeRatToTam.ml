open Tam
open Code
open Ast

type t1 = Ast.AstPlacement.programme
type t2 = String

(*Analyse l'expression*)
(*Renvoie le code Tam correspondant*)
let analyser_placement_expression exp =
	match exp with
		|AstType.Entier i -> loadl_int i
		|AstType.Booleen _ -> loadl_int 1
		|
		in
		
let rec analyser_placement_bloc (li, taillebloc) =
	begin
		let rec analyser_liste_instruction li =
			match li with
				|[] -> ""
				|t::q -> 
					|analyser_placement_instruction t
					+
					|analyser_liste_instruction q
				in
	(push (taillebloc))
	+
		(*Traiter les instructions de li*)
		analyser_liste_instruction li
	+
		pop (0) tailleBloc
	end

and analyser_placement_instruction i =
	match i with
		|AstPlacement.Declaration (iast, exp) ->
			push (getTaille (getType, iast))
			+
			|analyser_placement_expression exp
			+
			STORE (taille) @
		|AstPlacement.Affectation (iast, exp) ->
			|analyser_placement_expression exp
			+
			STORE (taille) @
		|AstPlacement.AffichageInt (exp) ->
			|analyser_placement_expression exp
			SUBR IOut
		|AstPlacement.AffichageBool(exp) ->
			|analyser_placement_expression exp
			+
			SUBR Bout
		|AstPlacement.AffichageRat (exp) ->
			|analyser_placement_expression exp
			+
			call (SB) ROut
		|AstPlacement.TantQue(exp, b) -> 
			let ld = getEtiquette() in
			let lf = getEtiquette() in
			ld
			+
			|analyser_placement_expressionression exp
			+
			JUMPIF (0) lf
			+
			analyser_bloc b
			+
			JUMP ld
			+
			lf
		|AstPlacement.Conditionnelle(exp, ba, be)
			let lfin = getEtiquette() in
			let lelse = getEtiquette() in
			+
			|analyser_placement_expressionression exp
			+
			JUMPIF (0) lelse
			+
			analyser_bloc ba
			+
			JUMP lfin
			+
			jelse
			+
			analyser_bloc be
			+
			lfin
		| AstPlacement.Empty -> ""
		| AstPlacement.Retour(exp, tailleRetour, tailleParam) ->
			|analyser_placement_expression exp
			+ || Il faudrait dépiler les variables à la fonction Mais le return s'en charge.
			RETURN (tailleRetour) tailleParam
