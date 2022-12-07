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
	| AstTds.Affichage (exp) -> failwith "not implemented"
	| AstTds.Conditionnelle (exp, li1, li2) -> AstType.Conditionnelle (analyser_type_expression exp, analyser_type_bloc li1, analyser_type_bloc li2)
	| AstTds.TantQue (exp, li) -> failwith "not implemented"
	| AstTds.Retour (exp, iast) -> AstType.Retour (analyser_type_expression exp, iast)
	| AstTds.Empty -> Empty



let analyser (AstTds.Programme (lf, b)) = 
	let mlf = analyser_type_fonction lf in
		let nb = analyser_type_bloc b in
			AstType.Programme (nlf, nb)
						