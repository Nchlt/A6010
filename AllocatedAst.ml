(* Syntaxe abstraite de la représentation intermédiaire après allocation *)
(* Le code est inchangé, mais la table des symboles donne l'emplacement de
   chaque registre virtuel. *)

module Symb_Tbl = IrAst.Symb_Tbl

type block       = IrAst.block
type instruction = IrAst.instruction
type literal     = IrAst.literal
type value       = IrAst.value
    
type alloc_info =
  | Reg   of string
  | Stack of int

type program = (string * function_info) list
and function_info = { 
  locals: alloc_info Symb_Tbl.t;
  offset: int;  (* Place à allouer sur la pile pour les variables locales *)
  code:   block
}
