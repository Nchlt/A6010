(* Arbre de syntaxe annoté par type *)
open SourceTypeChecker

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

type ( 'a, 'e ) annotated_element = { annot: 'a  ;
                                      elt  :  'e }

and typed_expression =  (typ,            S.expression)  annotated_element
and typed_location   =  (typ,            S.location)    annotated_element
and typed_call       =  ((typ option),   S.call )       annotated_element


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
  | ArrayAccess of a_access (* Case d'un tableau   *)

and a_access = typed_expression * typed_expression

(* Fonction pour comparer les types *)
let comparetype t1 t2 =
  not (t1 <> t2)

(* Les constructeurs *)

let mk_arrayaccess te1 te2 =
  let type_te1 = te1.annot in
  let type_te2 = te2.annot in
  match type_te1, type_te2 with
  | S.TypArray(t), S.TypInteger -> { annot = t; elt = ArrayAccess(te1, te2) }
  | _ -> failwith "ArrayAccess type error"


let mk_binop op te1 te2 =
  let type_te1 = te1.annot in
  let type_te2 = te2.annot in
  match (comparetype type_te1 type_te2) with
  | true -> { annot = type_te1 ; elt = Binop(op, te1, te2) }
  | _ -> failwith "Binop type error"

let mk_set tloc te =
  let type_tloc = tloc.annot  in
  let type_te  = te.annot     in
  match (comparetype type_tloc type_te) with
  | true -> { annot = type_tloc; elt = Set(tloc, te) }
  | _ -> failwith "Set type error"
