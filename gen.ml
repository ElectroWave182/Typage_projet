(* Compilation functions *)

open Lang
open Instrs
open Typing

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)

  
let gen_prog (Prog (fundecls, fundefns)) =
  ISeq []



let string_of_arithmeticOp =
function

    (* integer *)
    | BAadd -> "+"
    | BAsub -> "-"
    | BAmul -> "*"
    | BAdiv -> "/"
    | BAmod -> "%"
    
    (* float *)
    | BAfadd -> "+."
    | BAfsub -> "-."
    | BAfmul -> "*."
    | BAfdiv -> "/."

;;


let string_of_booleanOp =
function
    
    | BBand -> "et"
    | BBor -> "ou"

;;


let string_of_comparisonOp =
function
    
    | BCeq -> "="
    | BCge -> ">="
    | BCgt -> ">"
    | BCle -> "<="
    | BClt -> "<"
    | BCne -> "!="

;;


let string_of_operator =
function

    (* binary arithmetic operators *)
    | (BArith operateur) ->
        string_of_arithmeticOp operateur
    
    (* binary boolean operators: and, or *)
    | (BBool operateur) ->
        string_of_booleanOp operateur
    
    (* binary comparison operators: =, >=, >, <=, <, != *)
    | (BCompar operateur) ->
        string_of_comparisonOp operateur

;;


let rec gen_expr =
function

    (* constant *)
    | (Const valeur) ->
        IVal valeur
    
    (* variable *)
    | (VarE nomVar) ->
        IVar (Hashtbl.seeded_hash 0 nomVar)
    
    (* binary operation *)
    | (BinOp (operateur, exprA, exprB)) ->
        ISeq
        [
            gen_expr exprA;
            gen_expr exprB;
            IOper (string_of_operator operateur)
        ]
    
    (* conditional expr *)
    | (CondE (condition, blocA, blocB)) ->
        ISeq
        [
            gen_expr condition;
            IBloc (gen_expr blocA);
            IBloc (gen_expr blocB)
        ]
    
    (* application *)
    | (CallE (fonction, (parametres))) ->
    ISeq
    (
        gen_expr_list parametres
        @ [IOper fonction]
    )

and gen_expr_list =
function

    (* Cas récursif *)
    | (tete :: queue) ->
        gen_expr tete
        :: gen_expr_list queue
    
    (* Cas de base *)
    | _ -> []

;;



(* - Tests unitaires - *)


let a = FloatV 3.5 ;;
let b = IntV 0 ;;

let x = "x" ;;
let y = "y" ;;

let f = "f" ;;
let g = "g" ;;

let plus = BArith BAadd ;;
let ou = BBool BBor ;;

let expr1 = Const b ;;
let expr2 = VarE y ;;
let expr3 = CallE (g, []) ;;

let expr4 =
CondE
(
    BinOp
    (
        ou,
        Const
        (
            BoolV true
        ),
        expr2
    ),
    CondE
    (
        CallE
        (
            f,
            [
                Const
                (
                    IntV 4
                );
                Const
                (
                    IntV 2
                )
            ]
        ),
        VarE x,
        Const a
    ),
    BinOp
    (
        plus,
        expr1,
        expr3
    )
)
;;


gen_expr expr1 ;;
(* - : IVal (IntV 0) *)

gen_expr expr2 ;;
(* - : IVar 469249333 *)

gen_expr expr3 ;;
(* - : ISeq [IOper "g"] *)

gen_expr expr4 ;;
(* - : ISeq
 [ISeq [IVal (BoolV true); IVar 469249333; IOper "ou"];
  IBloc
   (ISeq
     [ISeq [IVal (IntV 4); IVal (IntV 2); IOper "f"]; IBloc (IVar 780510073);
      IBloc (IVal (FloatV 3.5))]);
  IBloc (ISeq [IVal (IntV 0); ISeq [IOper "g"]; IOper "+."])] *)
