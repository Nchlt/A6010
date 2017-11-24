(* Arbre de syntaxe annoté par type *)

module Symb_Tbl = SourceAst.Symb_Tbl

module S = SourceAst

type function_info    = S.function_info
type program          = S.program
type identifier_kind  = S.identifier_kind
type identifier_info  = S.identifier_info
type typ              = S.typ
type call             = S.call
type literal          = S.literal
type binop            = S.binop


type typed_expression =  typ     *      S.expression * annotated_element
type typed_location   =  typ     *      S.location   * annotated_element
type typed_call       = (typ option) *  S.call       * annotated_element

and 'a 'e annotated_element = { annot: 'a  ;
                                elt  :  'e }
and block = instruction list

and instruction =
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

and location =
  | Identifier  of string   (* Variable en mémoire *)
  | ArrayAccess of typed_a_access (* Case d'un tableau   *)

and a_access = typed_expression * typed_expression

(* Les constructeurs *)

let mk_arrayaccess te1 te2 =
  let (type_te1, _, _) = te1 in
  let (type_te2, _, _) = te2 in
  match type_te1, type_te2 with
  | TypeArray(t), TypeInteger ->
    { annot = t; elt: ArrayAccess(te1, te2) }
  | _ -> failwith "Found ArrayAccess violation"
