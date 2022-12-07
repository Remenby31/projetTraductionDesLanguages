open Type
open Ast.AstSyntax

(* Exceptions pour la gestion des identificateurs *)
exception DoubleDeclaration of string 
exception IdentifiantNonDeclare of string 
exception MauvaiseUtilisationIdentifiant of string 

(* Exceptions pour le typage *)
(* Le premier type est le type réel, le second est le type attendu *)
exception TypeInattendu of typ * typ
exception NombreParametresInattendus of int * int
exception TypesParametresInattendus of typ list * typ list
exception TypeBinaireInattendu of binaire * typ * typ (* les types sont les types réels non compatible avec les signatures connues de l'opérateur *)
exception AffichageUndefined of typ (* affichage d'un type non défini *)
exception BooleenAttenduWhile (* le test d'un while doit être un booléen *)
exception BooleenAttenduIf (* le test d'un if doit être un booléen *)
exception TypeInattenduRetour of typ (* le type réel du retour n'est pas compatible avec le type attendu *)

(* Utilisation illégale de return dans le programme principal *)
exception RetourDansMain
