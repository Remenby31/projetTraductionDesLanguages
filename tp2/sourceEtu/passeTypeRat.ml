open Tds
open Type
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme



	let getTypeExpression exp = match exp with
	| AstType.Ident iast -> getType iast (* On renvoie le type associé à l'ident *)
	| AstType.Entier _ -> Int (* Un entier renvoie un entier *)
	| AstType.Booleen _ -> Bool (* Un booleen renvoie un booleen *)
	| AstType.AppelFonction (iast, _) -> getType iast (* On renvoie le type associé à la fonction *)
	| AstType.Unaire _ -> Int (* Un Unaire renvoie un Entier *)
	| AstType.Binaire (op, _, _) ->
		begin
			match op with
			| AstType.PlusInt | AstType.MultInt  -> Int (* Renvoient du Int*)
			| AstType.PlusRat | AstType.MultRat | AstType.Fraction -> Rat (* Renvoient du Rat*)
			| AstType.EquInt | AstType.EquBool | AstType.Inf -> Bool (* Renvoient du booleen*)
		end


let rec analyser_type_expression e =
	match e with
	| AstTds.Entier i -> AstType.Entier i

	| AstTds.Booleen b -> AstType.Booleen b

	| AstTds.Ident iast -> AstType.Ident iast (* On renvoie le type de la variable *)

	| AstTds.AppelFonction (iast, le) -> 
		
	(* On vérifie que le iast est bien une fonction *)
	(* Si c'est une fonction, on vérifie que le type des paramètres est bien le même que celui de la fonction *)
		let nle = List.map analyser_type_expression le in (* On vérifie que les paramètres sont corrects *)

		let type_param_attendu = getParam iast in (* On récupère les paramètres de la fonction (attendus) *)
		let type_param_recu = List.map (getTypeExpression) nle in (* On récupère les types des paramètres de la fonction (reçus) *)

		(* On vérifie que les deux listes sont compatibles *)
		if est_compatible_list type_param_attendu type_param_recu 
			then AstType.AppelFonction (iast, nle) (* On renvoie le type de la fonction *)
		else raise (TypesParametresInattendus (type_param_attendu, type_param_recu)) (* On renvoie une erreur *)

	| AstTds.Binaire (op, e1, e2) -> 
		begin
			(* On vérifie que les deux expressions sont correctes *)
			let ne1 = analyser_type_expression e1 in
			let ne2 = analyser_type_expression e2 in 
			
			(* On récupère les types des expressions *)
			let t1 = getTypeExpression ne1 in
			let t2 = getTypeExpression ne2 in
			
			(* On teste si les deux expressions sont compatible avec l'opération *)
			match op with
				| AstSyntax.Fraction -> (* Si c'est une fraction*)
					if (est_compatible t1 Int) && (est_compatible t2 Int)
						then AstType.Binaire (AstType.Fraction, ne1, ne2) (* Fraction Int*)
					else raise (TypeBinaireInattendu (op, t1, t2))

				| AstSyntax.Plus -> (* Si c'est une addition*)
					if (est_compatible t1 Int) && (est_compatible t2 Int) (* Addition Int*)
						then AstType.Binaire (AstType.PlusInt, ne1, ne2)
					else if (est_compatible t1 Rat) && (est_compatible t2 Rat) (* Addition Rat*)
						then AstType.Binaire (AstType.PlusRat, ne1, ne2)
					else raise (TypeBinaireInattendu (op, t1, t2))

				| AstSyntax.Mult ->  (* Si c'est une multiplication*)
					if (est_compatible t1 Int) && (est_compatible t2 Int)
						then AstType.Binaire (AstType.MultInt, ne1, ne2) (* Multiplication Int*)
					else if (est_compatible t1 Rat) && (est_compatible t2 Rat) 
						then AstType.Binaire (AstType.MultRat, ne1, ne2) (* Multiplication Rat*)
					else raise (TypeBinaireInattendu (op, t1, t2))

				| AstSyntax.Equ ->  (* Si c'est un test d'égalité*)
					if ((est_compatible t1 Int) && (est_compatible t2 Int))
						then AstType.Binaire (AstType.EquInt, ne1, ne2) (* Test d'égalité Int*)
					else if ((est_compatible t1 Bool) && (est_compatible t2 Bool))
						then AstType.Binaire (AstType.EquBool, ne1, ne2) (* Test d'égalité Bool*)
					else raise (TypeBinaireInattendu (op, t1, t2))

				| AstSyntax.Inf -> (* Si c'est un test d'infériorité*)
					if ((est_compatible t1 Int) && (est_compatible t2 Int)) 
						then AstType.Binaire (AstType.Inf, ne1, ne2) (* Test d'infériorité Int*)
					else raise (TypeBinaireInattendu (op, t1, t2))
				end

	| AstTds.Unaire (op, e) ->
		begin
		(* On vérifie que l'expression est correcte *)
		let ne = analyser_type_expression e in

		(* On récupère le type de l'expression *)
		let t = getTypeExpression ne in

		(* On teste si l'expression est compatible avec l'opération (C'est un Rationnel) *)
		if (est_compatible t Rat) then
			AstType.Unaire (convertirUnaire op, ne) (* Si c'est un Rat, on renvoie l'expression *)
		else
			raise (TypeInattendu (t, Rat)) (* Si ce n'est pas un entier, on lève une exception *)
		end
	

(* AstTds.programme -> ASTType.programme *)
let rec analyser_type_bloc li = List.map analyser_type_instruction li 

(* AstTds.contructeur -> ASTType.instructeur*)
and analyser_type_instruction i =
	match i with
	| AstTds.Declaration (typ, iast, e) ->
		(* On vérifie que l'expression est correcte *)
		let ne = analyser_type_expression e in
		
		(* On récupère le type de l'expression *)
		let t = getTypeExpression ne in
		
		(* On teste si le type de l'expression est compatible avec le type de la variable *)
		if (est_compatible t typ) then
			begin
			modifier_type_variable typ iast; (* On modifie le type de la variable *)
			AstType.Declaration (iast, ne) (* Si les types sont les mêmes, on renvoie l'instruction *)
			end
		else
			raise (TypeInattendu (t, typ)) (* Si les types sont différents, on lève une exception *)

	| AstTds.Affectation (iast, e) -> 
		(* On vérifie que l'expression est correcte *)
		let ne = analyser_type_expression e in
		
		(* On récupère le type de l'expression et le type de la variable*)
		let t = getTypeExpression ne in
		let t_attendu = getType iast in

		(* On teste si le type de l'expression est compatible avec le type de la variable *)
		if (est_compatible t t_attendu) then
			AstType.Affectation (iast, ne) (* Si les types sont les mêmes, on renvoie l'instruction *)
		else
			raise (TypeInattendu (t, t_attendu)) (* Si les types sont différents, on lève une exception *)

	| AstTds.Affichage (e) -> 
		begin
		(* On vérifie que l'expression est correcte *)
		let ne = analyser_type_expression e in
		
		(* On récupère le type de l'expression *)
		let t = getTypeExpression ne in

		(* On différencie les différents affichages*)
		match t with
		| Int -> AstType.AffichageInt (ne) (* 1. Affichage des Entiers *)
		| Bool -> AstType.AffichageBool (ne) (* 2. Affichage des Booléens *)
		| Rat -> AstType.AffichageRat (ne) (* 3. Affichage des fractions (binaire) *)
		| _ -> raise (AffichageUndefined t) (* 4. Affichage non supporté *)
		end

	| AstTds.Conditionnelle (e, li1, li2) -> 
		(* On vérifie que l'expression est correcte *)
		let ne = analyser_type_expression e in
		
		(* On récupère le type de l'expression *)
		let t = getTypeExpression ne in

		(* On vérifie que l'expression est bien un booléen *)
		if (est_compatible t Bool) then
			AstType.Conditionnelle (ne, analyser_type_bloc li1, analyser_type_bloc li2) (* Si c'est un booléen, on renvoie l'instruction *)
		else
			raise (TypeInattendu (t, Bool)) (* Si ce n'est pas un booléen, on lève une exception *)

	| AstTds.TantQue (e, li) -> 
			(* On vérifie que l'expression est correcte *)
			let ne = analyser_type_expression e in
		
			(* On récupère le type de l'expression *)
			let t = getTypeExpression ne in
	
			(* On vérifie que l'expression est bien un booléen *)
			if (est_compatible t Bool) then
				AstType.TantQue (ne, analyser_type_bloc li) (* Si c'est un booléen, on renvoie l'instruction *)
			else
				raise (TypeInattendu (t, Bool)) (* Si ce n'est pas un booléen, on lève une exception *)

	| AstTds.Retour (e, iast) -> 
		(* On vérifie que l'expression est correcte *)
		let ne = analyser_type_expression e in
	
		(* On récupère le type de l'expression et le type de retour *)
		let t = getTypeExpression ne in
		let type_attendu = getType iast in

		(* On teste si le type de l'expression est compatible avec le type de retour *)
		if (est_compatible type_attendu t) then
			AstType.Retour (ne, iast) (* Si les types sont les mêmes, on renvoie l'expression *)
		else
			raise (TypeInattendu (t, type_attendu)) (* Si les types sont différents, on lève une exception *)

	| Empty -> AstType.Empty

let analysefonction (AstTds.Fonction (typ, iast, lparam, bloc)) = 
	begin
		(* On modifie les types dans paramestre dans leur iast *)
		let nlparam = List.map (fun (typv, iastv) -> modifier_type_variable typv iastv ; iastv) lparam in
		
		(* On récupère les types des paramètres *)
		let type_param = List.map (fun (typv, _) -> typv) lparam in

		(*
		let f_verif t =
			if t = Undefined then
				failwith "Erreeeeeeeeuuuuuuuuuurrrrrrrr"
			else
				()

		in let _ = List.map f_verif type_param in
		*)

		(* On vérifie que le type de retour est correct *)
		let nbloc = analyser_type_bloc bloc in 
		
		 (* On modifie le type de la fonction *)
		modifier_type_fonction typ type_param iast;
		
		(* On renvoie la fonction *)
		AstType.Fonction (iast, nlparam, nbloc)
	end


(* AstTds.programme -> ASTType.programme *)


let analyser (AstTds.Programme (fonctions, prog)) = 
	let nf = List.map analysefonction fonctions in
	let np = analyser_type_bloc prog in
	AstType.Programme (nf, np)
						