open Type
open Exceptions
open Ast

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

let analyser (AstType.Programme (nf, np)) =  