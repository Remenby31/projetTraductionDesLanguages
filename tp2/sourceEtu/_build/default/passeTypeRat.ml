open Tds
open Type
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme



let rec getTypeExpression exp = match exp with
| AstTds.Ident iast -> getType iast
| AstTds.Entier _ -> Int
| AstTds.Booleen _ -> Bool
| AstTds.AppelFonction (iast, _) -> getType iast
| AstTds.Unaire _ -> Int
| AstTds.Binaire (op, e1, e2) ->
	begin
		let t1 = getTypeExpression e1 in
		let t2 = getTypeExpression e2 in
		(*On teste si les deux expressions sont du même type*)
		if t1 = t2 then 
			match op with
			| AstSyntax.Plus -> t1 (*on renvoie le type des expression*)
			| AstSyntax.Mult -> t1  (*on renvoie le type des expression*)
			| AstSyntax.Equ -> Bool (*C'est un test d'égalité, on renvoie un booleen*)
			| AstSyntax.Inf -> Bool (*C'est un test d'infériorité, on renvoie un booleen*)
			| AstSyntax.Fraction -> Rat (*C'est une fraction, on renvoie un rationnel*)
		else raise (TypeBinaireInattendu (op, t1, t2)) (* les deux expressions ne sont pas du même type, on lève une exception *)
	end
	

let rec analyser_type_expression e =
	match e with
	| AstTds.Entier i -> AstType.Entier i
	| AstTds.Booleen b -> AstType.Booleen b
	| AstTds.Ident iast -> AstType.Ident iast
	| AstTds.AppelFonction (iast, le) -> 
		begin
	(* On vérifie que le iast est bien une fonction *)
	(* Si c'est une fonction, on vérifie que le type des paramètres est bien le même que celui de la fonction *)
		let lparamtype_attendu = getParam iast in (* On récupère les types des paramètres de la fonction (attendus) *)
		let lparamtype_recu = List.map getTypeExpression le in (* On récupère les types des paramètres de la fonction (reçus) *)
		try if (List.for_all2 est_compatible lparamtype_attendu lparamtype_recu) then
			AstType.AppelFonction (iast, List.map analyser_type_expression le)
		else
			(* Si le type des paramètres est différent, on lève une exception *)
			raise (TypesParametresInattendus (lparamtype_attendu, lparamtype_recu))
		(* Si le nombre de paramètres est différent, on lève une exception *)
		with Invalid_argument _ -> raise (NombreParametresInattendus (List.length lparamtype_attendu, List.length lparamtype_recu))
		end
	| AstTds.Binaire (op, e1, e2) -> 
		begin
			let t1 = getTypeExpression e1 in
			let t2 = getTypeExpression e2 in
			let ne1 = analyser_type_expression e1 in
			let ne2 = analyser_type_expression e2 in
			(*On teste si les deux expressions sont correctes*)
			match op with
				| AstSyntax.Fraction -> (* On teste si c'est une fraction*)
					if (est_compatible t1 Int) && (est_compatible t2 Int)
						then AstType.Binaire (AstType.Fraction, ne1, ne2) (* Fraction Int*)
					else raise (TypeInattendu (t1, Int))
				| AstSyntax.Plus -> (* On teste si c'est une addition*)
					if (est_compatible t1 Int) && (est_compatible t2 Int) (* Addition Int*)
						then AstType.Binaire (AstType.PlusInt, ne1, ne2)
					else if (est_compatible t1 Rat) && (est_compatible t2 Rat) (* Addition Rat*)
						then AstType.Binaire (AstType.PlusRat, ne1, ne2)
					else raise (TypeBinaireInattendu (op, t1, t2))
				| AstSyntax.Mult ->  (* On teste si c'est une multiplication*)
					if (est_compatible t1 Int) && (est_compatible t2 Int)
						then AstType.Binaire (AstType.MultInt, ne1, ne2) (* Multiplication Int*)
					else if (est_compatible t1 Rat) && (est_compatible t2 Rat) 
						then AstType.Binaire (AstType.MultRat, ne1, ne2) (* Multiplication Rat*)
					else raise (TypeBinaireInattendu (op, t1, t2))
				| AstSyntax.Equ -> 
					if ((est_compatible t1 Int) && (est_compatible t2 Int)) (* On teste si c'est un test d'égalité*)
					then AstType.Binaire (AstType.EquInt, ne1, ne2) (* Test d'égalité Int*)
					else if ((est_compatible t1 Bool) && (est_compatible t2 Bool))
					then AstType.Binaire (AstType.EquBool, ne1, ne2) (* Test d'égalité Bool*)
					else raise (TypeBinaireInattendu (op, t1, t2))
				| AstSyntax.Inf -> 
					if ((est_compatible t1 Int) && (est_compatible t2 Int)) (* On teste si c'est un test d'infériorité*)
					then AstType.Binaire (AstType.Inf, ne1, ne2) (* Test d'infériorité Int*)
					else raise (TypeBinaireInattendu (op, t1, t2))
				end
	| AstTds.Unaire (op, e) ->
		begin
		(* On vérifie que le type de l'expression est bien un entier *)
		let t = getTypeExpression e in
		if (est_compatible t Rat) then
			AstType.Unaire (convertirUnaire op, analyser_type_expression e) (* Si c'est un entier, on renvoie l'expression *)
		else
			raise (TypeInattendu (t, Rat)) (* Si ce n'est pas un entier, on lève une exception *)
		end
	

(* AstTds.programme -> ASTType.programme *)
let rec analyser_type_bloc li = List.map analyser_type_instruction li

(* AstTds.contructeur -> ASTType.instructeur*)
and analyser_type_instruction i =
	match i with
	| AstTds.Declaration (typ, iast, exp) ->
		(* On vérifie que le type de l'expression est bien le même que celui de la déclaration *)
		let t = getTypeExpression exp in
		if (est_compatible t typ) then
			begin
			modifier_type_variable typ iast; (* On modifie le type de la variable *)
			AstType.Declaration (iast, analyser_type_expression exp) (* Si les types sont les mêmes, on renvoie l'instruction *)
			end
		else
			raise (TypeInattendu (t, typ)) (* Si les types sont différents, on lève une exception *)
	| AstTds.Affectation (iast, exp) -> 
		(* On vérifie que le type de l'expression est bien le même que celui de l'affectation *)
		let t_exp = getTypeExpression exp in
		let t_iast = getType iast in
		if (est_compatible t_exp t_iast) then
			AstType.Affectation (iast, analyser_type_expression exp) (* Si les types sont les mêmes, on renvoie l'instruction *)
		else
			raise (TypeInattendu (t_exp, t_iast)) (* Si les types sont différents, on lève une exception *)
	| AstTds.Affichage (exp) -> 
		begin
		(* On différencie les différents affichages*)
		match exp with
		| AstTds.Entier i -> AstType.AffichageInt (AstType.Entier i) (* 1. Affichage des Entiers *)
		| AstTds.Booleen b -> AstType.AffichageBool (AstType.Booleen b) (* 2. Affichage des Booléens *)
		| AstTds.Binaire _ -> AstType.AffichageRat (analyser_type_expression exp) (* 3. Affichage des fractions (binaire) *)
		| _ -> raise (AffichageUndefined (getTypeExpression exp)) (* 4. Affichage non supporté *)
		end
	| AstTds.Conditionnelle (exp, li1, li2) -> 
		(* On vérifie que l'expression est bien un booléen *)
		if (getTypeExpression exp = Bool) then
			AstType.Conditionnelle (analyser_type_expression exp, analyser_type_bloc li1, analyser_type_bloc li2)
		else
			raise (TypeInattendu (getTypeExpression exp, Bool))
	| AstTds.TantQue (exp, li) -> 
		(* On vérifie que l'expression est bien un booléen *)
		if (getTypeExpression exp = Bool) then
			AstType.TantQue (analyser_type_expression exp, analyser_type_bloc li)
		else
			raise (TypeInattendu (getTypeExpression exp, Bool))
	| AstTds.Retour (exp, iast) -> 
		begin
		(* On vérifie que le type de l'expression est bien le même que celui de la fonction *)
		let t = getType iast in
		if (est_compatible t (getTypeExpression exp)) then
			AstType.Retour (analyser_type_expression exp, iast) (* Si les types sont les mêmes, on renvoie l'expression *)
		else
			raise (TypeInattendu (t, getTypeExpression exp)) (* Si les types sont différents, on lève une exception *)
		end
		| Empty -> AstType.Empty

let analysefonction (AstTds.Fonction (typ, iast, lparam, bloc)) = 
	(* On vérifie que le type de la fonction est bien le même que celui de la fonction *)
	let t = getType iast in
		if (est_compatible t typ) then
			(* On vérifie les types des parametres *)
			let ltype_attendus = List.map (fun (typ, _) -> typ) lparam in (* On récupère les types des parametres attendus *)
			let ltype_recus = List.map (fun (_, iast) -> getType iast) lparam in (* On récupère les types des parametres reçus *)
			(* Fonction verifierparam qui verifie que deux parametres *)
			let verifierparam (typ, iast) =
				if (est_compatible typ (getType iast)) then
					iast
				else
					(* Si le type du parametre n'est pas le même que celui de la fonction, on lève une exception *)
					raise (TypesParametresInattendus (ltype_attendus, ltype_recus))
				in
			let nlparam = List.map (fun (typ, iast) -> (verifierparam (typ, iast))) lparam in (* On vérifie les parametres *)
			let nbloc = analyser_type_bloc bloc in (* On vérifie le bloc *)
			begin
				modifier_type_fonction typ ltype_attendus iast; (* On modifie le type de la fonction *)
				AstType.Fonction (iast, nlparam, nbloc) (* Si les types sont les mêmes, on renvoie la fonction *)
			end
		else
			(* Si le type de la fonction n'est pas le même que celui de la fonction, on lève une exception *)
			raise (TypeInattendu (t, typ))


let analyser (AstTds.Programme (fonctions, prog)) = 
	let nf = List.map analysefonction fonctions in
	let np = analyser_type_bloc prog in
	AstType.Programme (nf, np)
						