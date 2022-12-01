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
	| AstTds.Binaire (op, e1, e2) -> AstType.Binaire (op, analyser_type_expression e1, analyser_type_expression e2)
	| AstTds.Unaire (op, e) -> AstType.Unaire (op, analyser_type_expression e)


let rec analyser_type_bloc li = List.map analyser_type_instruction li
(* AstTds.contructeur -> ASTType.instructeur*)
and analyser_type_instruction i =
	match i with
		AstTds.Declaration (t, rast, exp) ->
			let (te, ne) = analyser_type_expression exp in
			if (t = te)
				then modifier_type_variable t rast;
					ASTType.Declaration (rast, ne)
				else raise TypeInattendu (t, te)
		AstTds.Affectation (rast, exp) ->
			let t = getType rast in
			let (te, ne) = analyser_type_expression exp in
			if (t = te)
				then ASTType.Affectation(rast,exp)
				else raise TypeInattendu
			end if;
		AstTds.Affichage(rast, exp) ->
		let (te, ne) = analyser_type_expression exp in
			match te with
				Int -> AffichageInt ne
				Rat -> AffichageRat ne
				Bool -> AffichageBool ne
				_ -> failwith "Erreur interne"
		AstTds.Conditionnelle(c, bt, be) ->
			let analyser_type_expression c in
				if (tc = Bool)
					then let nbt = analyser_type_bloc bt in
						let nbe = analyser_type_bloc bt in
						let nbe = analyser_type_bloc be in
						ASTType.Conditionnelle(nc, nbt, nbe)
		AstTds.Retour(inst, exp)->
				let t = getTypeRetour inst in
				let (te, ne ) = analyser_type_instruction
				if (t = te)
					then ASTType.Retour(inst, ne)
					else raise TypeInattendu...


let analyser (AstTds.Programme (lf, b)) = 
	let mlf = analyser_type_fonction lf in
		let nb = analyser_type_bloc b in
			AST Type.Programme (nlf, nb)
						