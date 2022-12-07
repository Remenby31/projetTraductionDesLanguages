open Tds
open Type
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


let rec analyser_type_expression e =
	match e with
	| AstTds.Entier i -> AstType.Entier i
	| AstTds.Booleen b -> AstType.Booleen b
	| AstTds.Ident iast -> AstType.Ident iast
	| AstTds.AppelFonction (iast, le) -> AstType.AppelFonction (iast, List.map analyser_type_expression le)
	| AstTds.Binaire (op, e1, e2) -> 
		begin
		let nop = match op with 
		| AstSyntax.Fraction -> AstType.Fraction
 	 	| AstSyntax.Plus -> AstType.PlusInt
  	| AstSyntax.Mult -> AstType.MultInt
  	| AstSyntax.Equ -> AstType.EquInt
  	| AstSyntax.Inf -> AstType.Inf
			in AstType.Binaire (nop, analyser_type_expression e1, analyser_type_expression e2)
		end
	| AstTds.Unaire (op, e) ->
		begin
			let nop = match op with
			| AstSyntax.Numerateur -> AstType.Numerateur
			| AstSyntax.Denominateur -> AstType.Denominateur
		in AstType.Unaire (nop, analyser_type_expression e)
	end


let rec analyser_type_bloc li = List.map analyser_type_instruction li
(* AstTds.contructeur -> ASTType.instructeur*)
and analyser_type_instruction i =
	match i with
	| AstTds.Declaration (typ, iast, exp) ->
		let t = getType iast in
			if (est_compatible t typ) then
				AstType.Declaration (iast, analyser_type_expression exp)
			else
				raise (TypeInattendu (t, typ))
	| AstTds.Affectation (iast, exp) -> AstType.Affectation (iast, analyser_type_expression exp)
	| AstTds.Affichage (exp) -> 
		begin
		(* On différencie les différents affichages*)
		match exp with
		| AstTds.Entier i -> AstType.AffichageInt (AstType.Entier i) (* affichage des Entiers *)
		| AstTds.Booleen b -> AstType.AffichageBool (AstType.Booleen b) (* affichage des Booléens *)
		| AstTds.Unaire (u, exp) -> (* affichage des fractions (unaires) *)
			begin
				(* On différencie les numérateurs et les dénominateurs en les convertissant en AstType*)
			let nu = match u with
			| AstSyntax.Numerateur -> AstType.Numerateur
			| AstSyntax.Denominateur -> AstType.Denominateur
			in
		AstType.AffichageInt (AstType.Unaire (nu, analyser_type_expression exp))
		end
		(* affichage non supporté *)
		| _ -> raise AffichageUndefined
		end
	| AstTds.Conditionnelle (exp, li1, li2) -> AstType.Conditionnelle (analyser_type_expression exp, analyser_type_bloc li1, analyser_type_bloc li2)
	| AstTds.TantQue (exp, li) -> 
		begin
			(* On vérifie que l'expression est bien un booléen *)
		match exp with 
		| AstTds.Booleen b -> AstType.TantQue (analyser_type_expression exp, analyser_type_bloc li)
		| _ -> raise BooleenAttenduWhile 
		end
	| AstTds.Retour (exp, iast) -> 
		begin
		(* On vérifie que le type de l'expression est bien le même que celui de la fonction *)
		let t = getType iast in
		match exp with
		(* Si c'est un booleen*)
		| AstTds.Booleen b ->
			begin
				if (est_compatible t Bool) then
					(AstType.Retour (analyser_type_expression exp, iast))
				else
					raise (TypeInattendu (t, Bool))
			end
			(* Si c'est un entier *)
		| AstTds.Entier i ->
			begin
				if (est_compatible t Int) then
					(AstType.Retour (analyser_type_expression exp, iast))
				else
					raise (TypeInattendu (t, Int))
			end
			(* Si c'est une fraction *)
		| AstTds.Unaire (u, exp) ->
			begin
				if (est_compatible t Rat) then
					(AstType.Retour (analyser_type_expression exp, iast))
				else
					raise (TypeInattendu (t, Rat))
			end
			(* Si c'est autre chose *)
		| _ -> raise (TypeInattenduRetour (getType iast))
		end
		| _ -> failwith "Instruction non supportée"

let analyser (AstTds.Programme (lf, b)) = 
	let mlf = analyser_type_fonction lf in
		let nb = analyser_type_bloc b in
			AstType.Programme (nlf, nb)
						