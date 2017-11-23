(* Arbre de syntaxe annoté par type *)

module Symb_Tbl = SourceAst.Symb_Tbl

open SourceAst
(*
type function_info    = SourceAst.function_info
type program          = SourceAst.program
type identifier_kind  = SourceAst.identifier_kind
type identifier_info  = SourceAst.identifier_info
type typ              = SourceAst.typ
type block            = SourceAst.block
type instruction      = SourceAst.instruction (*x*)
type expression       = SourceAst.expression  (*x*)
type call             = SourceAst.call        (*x*)
type literal          = SourceAst.literal
type location         = SourceAst.location
type binop            = SourceAst.binop
*)

type typed_expression =  typ           expression  annotated_element
type typed_location   =  typ           location    annotated_element
type typed_call       = (typ option)   call        annotated_element

and 'a 'e annotated_element = { annot: 'a  ;
                                elt  :  'e }

and instructions =
  | Set   of typed_location   * typed_expression    (* Affectation       *)
  | While of typed_expression * block               (* Boucle            *)
  | If    of typed_expression * block * block       (* Branchement       *)
  | Print of typed_expression                       (* Affichage         *)
  | ProcCall of typed_call                          (* Appel de fonction *)

and expression =
  | Literal   of literal                                      (* Valeur immédiate    *)
  | Location  of typed_location                               (* Valeur en mémoire   *)
  | Binop     of binop * typed_expression * typed_expression  (* Opération binaire   *)
  | FunCall   of typed_call                                   (* Appel de fonction   *)
  | NewArray  of typed_expression * typ                       (* Création de tableau *)

and a_access = typed_expression * typed_expression
