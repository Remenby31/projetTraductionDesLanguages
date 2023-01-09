open Tam
open Code
open Ast
open Tds
open Type

type t1 = Ast.AstPlacement.programme
type t2 = string

(*Analyse l'expression*)
(*Renvoie le code Tam correspondant*)
let rec analyser_code_expression exp =
	match exp with
		|AstType.AppelFonction (ia, le) ->
			(* On analyse les arguments *)
			let rec analyser_liste_expression le =
				begin
				match le with
					|[] -> ""
					|t::q -> 
						analyser_code_expression t
						^
						analyser_liste_expression q
				end
				in
			(analyser_liste_expression le )
			^
			(call ("SB") (get_nom_fonction ia))

		|AstType.Ident ia -> 
			let (d, b) = get_adresse_variable ia in
			let taille = getTaille (getType ia) in
			(load taille d b)
		
		|AstType.Booleen b -> loadl_int (if b then 1 else 0)
		|AstType.Entier i -> loadl_int i
		|AstType.Unaire (op, exp) -> 
			(analyser_code_expression exp)
			^
			(match op with
				|AstType.Numerateur -> "" (*On ne fait rien car le numerateur est déjà sur la pile*)
				|AstType.Denominateur -> pop (0) 1 (*On enlève le numerateur*)
			)
		| AstType.Binaire (op, exp1, exp2) ->
			(analyser_code_expression exp1)
			^
			(analyser_code_expression exp2)
			^
			(match op with
				|AstType.Fraction -> "" (*On ne fait rien car la fraction est déjà sur la pile*)
				|AstType.PlusInt -> (*On fait l'addition*)
					(call "SB" "IAdd")
				|AstType.PlusRat -> (*On fait l'addition*)
					(call "SB" "RAdd")
				|AstType.MultInt ->  (*On fait la multiplication*)
					(call "SB" "IMult")
				|AstType.MultRat -> (*On fait la multiplication*)
					(call "SB" "RMult")
				|AstType.EquInt -> (*On fait la comparaison*)
					(call "SB" "IEq")
				|AstType.EquBool -> (*On fait la comparaison*)
					(call "SB" "BEq")
				|AstType.Inf -> (*On fait la comparaison*)
					(call "SB" "ILt")
			)




let rec analyser_code_bloc (li, taillebloc) =
		let rec analyser_liste_instruction li =
			begin
			match li with
				|[] -> ""
				|t::q -> 
					(analyser_code_instruction t)
					^
					(analyser_liste_instruction q)
			end
			in
		(push taillebloc)
	^
		(*Traiter les instructions de li*)
		(analyser_liste_instruction li)
	^
		(pop (0) taillebloc)

and analyser_code_instruction i =
	match i with
		|AstPlacement.Declaration (iast, exp) ->

			let taille = getTaille (getType iast) in
			let (d, b) = get_adresse_variable iast in
			(push (taille))
			^
			(analyser_code_expression exp)
			^
			(store taille d b)

		|AstPlacement.Affectation (iast, exp) ->

			let taille = getTaille (getType iast) in
			let (d, b) = get_adresse_variable iast in
			(push (taille))
			^
			(analyser_code_expression exp)
			^
			(store taille d b)

		|AstPlacement.AffichageInt (exp) ->

			(analyser_code_expression exp)
			^
			(subr "IOut")

		|AstPlacement.AffichageBool(exp) ->

			(analyser_code_expression exp)
			^
			(subr "BOut")

		|AstPlacement.AffichageRat (exp) ->

			(analyser_code_expression exp)
			^
			(call "SB" "ROut")

		|AstPlacement.TantQue(exp, b) -> 

			let ld = getEtiquette() in (*Etiquette de début de la boucle*)
			let lf = getEtiquette() in (*Etiquette de fin de la boucle*)

			ld ^ "\n"
			^
			analyser_code_expression exp
			^
			jumpif 0 lf
			^
			analyser_code_bloc b
			^
			jump ld
			^
			lf ^ "\n"
		|AstPlacement.Conditionnelle(exp, ba, be) ->
			let lfin = getEtiquette() in (*Etiquette de fin de la conditionnelle*)
			let lelse = getEtiquette() in (*Etiquette de fin de la conditionnelle*)
			analyser_code_expression exp
			^
			jumpif (0) lelse
			^
			analyser_code_bloc ba
			^
			jump lfin
			^
			lelse ^ "\n"
			^
			analyser_code_bloc be
			^
			lfin ^ "\n"
		
		| AstPlacement.Retour(exp, tailleRetour, tailleParam) ->
			analyser_code_expression exp
			^
			(*^ || Il faudrait dépiler les variables à la fonction Mais le return s'en charge.*)
			return (tailleRetour) tailleParam
		| AstPlacement.Empty -> ""

let analyser_code_fonction (AstPlacement.Fonction (_, _, b)) =
	(* On recupere les parametres de la fonction *)
	let lb = getEtiquette() in 
	let nb = analyser_code_bloc b in
	lb ^ "\n" 
	^
	nb

