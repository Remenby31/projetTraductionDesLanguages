open Type
open Ast
open Tds

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme


(* modifier_adresse_variable : int -> string -> AstType.ident -> unit *)
(* Modifie l'adresse d'une variable dans la table des symboles *)
let rec analyse_placement_bloc li reg depl =
  match li with
    | [] -> ([],0)
    | i::q ->
      let (ni, taille) = analyser_placement_instruction i reg depl in
      let (nq, taille_q) = analyse_placement_bloc q reg (depl + taille) in
      (ni::nq, taille + taille_q)

(* analyser_placement_instruction : AstType.instruction -> AstPlacement.instruction *)
(* Analyse une instruction et place les variables dans la mémoire *)
and analyser_placement_instruction i reg depl =
  match i with
    |AstType.Declaration(iast, exp) ->
      begin
        modifier_adresse_variable depl reg iast;
        AstPlacement.Declaration (iast, exp), getTaille(getType iast)
      end
    |AstType.Affectation(iast, exp) -> (AstPlacement.Affectation(iast, exp),0)
    |AstType.AffichageInt(exp) -> (AstPlacement.AffichageInt(exp),0)
    |AstType.AffichageRat(exp) -> (AstPlacement.AffichageRat(exp),0)
    |AstType.AffichageBool(exp) -> (AstPlacement.AffichageBool(exp),0)
    |AstType.Conditionnelle(exp, bt, be) ->
      let nbt = analyse_placement_bloc bt reg depl in
      let nbe = analyse_placement_bloc be reg depl in
        (AstPlacement.Conditionnelle(exp, nbt,nbe),0)
    |AstType.TantQue(exp, b) -> 
      let nb = analyse_placement_bloc b reg depl in
      (AstPlacement.TantQue(exp, nb),0)
    |AstType.Retour(exp, iast) -> 
      let type_param = getParam iast in
      let taille_param = List.fold_left (fun acc x -> acc + getTaille x) 0 type_param in
      (AstPlacement.Retour(exp, getTaille (getType iast), taille_param), 0)
    |AstType.Empty -> (AstPlacement.Empty,0)

(* analyser_placement_fonction : AstType.fonction -> AstPlacement.Fonction *)
(* Analyse une fonction et place les variables dans la mémoire *)
and analyser_placement_fonction (AstType.Fonction (iast, iast_param, b)) =
  (* On place les paramètres de la fonction *)
  let rec placer_variable_param li depl =
    begin
      match li with
        | [] -> ()
        | i::q -> 
          let ndepl = depl - getTaille (getType i) in
          modifier_adresse_variable ndepl "LB" i;
          placer_variable_param q ndepl
    end
  in
  let nb = analyse_placement_bloc b "LB" 3 in
  placer_variable_param iast_param 0;
  AstPlacement.Fonction(iast, iast_param, nb)

(* analyser : AstType.programme -> AstPlacement.Programme *)
(* Analyse le programme et place les variables dans la mémoire *)
let analyser (AstType.Programme (lf, b)) = 
  let nlf = List.map (analyser_placement_fonction) lf in
  let nb = analyse_placement_bloc b "SB" 0 in
  AstPlacement.Programme (nlf, nb)