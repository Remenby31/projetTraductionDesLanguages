open Tam
open Code
open Ast
open Tds
open Type

type t1 = Ast.AstPlacement.programme
type t2 = String

(*Analyse l'expression*)
(*Renvoie le code Tam correspondant*)
let analyser_code_expression exp =
	match exp with
		|AstType.Entier i -> loadl_int i
		|AstType.Booleen _ -> loadl_int 1
		| _ -> failwith "Erreur : Expression non reconnue"

		
let rec analyser_rat_bloc (li, taillebloc) =
		let rec analyser_liste_instruction li =
			begin
			match li with
				|[] -> ""
				|t::q -> 
					analyser_liste_instruction q
			end
			in
		(push taillebloc)
	+
		(*Traiter les instructions de li*)
		(analyser_liste_instruction li)
	+
		(pop (0) tailleBloc)

and analyser_placement_instruction i =
	match i with
		|AstPlacement.Declaration (iast, exp) ->

			let taille = getTaille (getType iast) in
			let (d, b) = get_adresse_variable iast in
			push (taille)
			+
			analyser_code_expression exp
			+
			store taille d b

		|AstPlacement.Affectation (iast, exp) ->

			let taille = getTaille (getType iast) in
			let (d, b) = get_adresse_variable iast in
			push (taille)
			+
			analyser_code_expression exp
			+
			store taille d b

		|AstPlacement.AffichageInt (exp) ->
			analyser_code_expression exp
			subr "IOut"
		|AstPlacement.AffichageBool(exp) ->
			analyser_code_expression exp
			+
			subr "Bout"
		|AstPlacement.AffichageRat (exp) ->
			analyser_code_expression exp
			+
			call (SB) ROut
		|AstPlacement.TantQue(exp, b) -> 
			let ld = getEtiquette() in
			let lf = getEtiquette() in
			ld
			+
			analyser_code_expression exp
			+
			jumpif (0) lf
			+
			analyser_bloc b
			+
			jump ld
			+
			lf
		|AstPlacement.Conditionnelle(exp, ba, be)
			let lfin = getEtiquette() in
			let lelse = getEtiquette() in
			+
			analyser_code_expression exp
			+
			jumpif (0) lelse
			+
			analyser_bloc ba
			+
			jump lfin
			+
			jelse
			+
			analyser_bloc be
			+
			lfin
		| AstPlacement.Empty -> ""
		| AstPlacement.Retour(exp, tailleRetour, tailleParam) ->
			analyser_code_expression exp
			(*+ || Il faudrait dépiler les variables à la fonction Mais le return s'en charge.*)
			return (tailleRetour) tailleParam
		| _ -> failwith "Erreur : Instruction non reconnue"
