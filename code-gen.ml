#use "semantic-analyser.ml";;
exception X_syntax_error;;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "SOB_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* This signature represents the idea of outputing assembly code as a string
     for a single AST', given the full constants and fvars tables. 
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

let type_size = 1;;
let word_size = 8;;
let num_char_size = (type_size + word_size) ;;

let rec const_table_maker listOfExprs = 
  let constSexprsList = ( findConsts listOfExprs ) in
  let dupelessConstList = ( cleanDupes constSexprsList ) in
  let tupledList = ( tupleListMaker dupelessConstList ) in
  let basicList = [ (Void, (0, "MAKE_VOID"));                  (Sexpr(Nil), (1, "MAKE_NIL"));
                    (Sexpr(Bool(false)), (2, "MAKE_BOOL(0)")); (Sexpr(Bool(true)), (4, "MAKE_BOOL(1)")) ] in 
  let offsetFixedList = offsetFixer  ( List.append basicList tupledList ) in
  offsetFixedList

and findConsts listOfExpr = 
match listOfExprs with
  | []     -> []
  | a :: b -> ( List.append (constScanner a) (findConsts b) ) 

(* TEST:  cleanDupes [1;1;1;2;3;4;5;4;3;3;6;2;2]  
TODO: might not know how to equalise objects - use expr'_eq instead *)

and cleanDupes sexprList = 
  match sexprList with
  | []      -> []
  | a :: b  -> (
    match (List.mem a b) with
    | true  -> ( cleanDupes b )
    | false -> ( List.append [a] (cleanDupes b) )
    )

and tupleListMaker sexprsList = 
  match sexprsList with
  | []            -> []
  | a :: b        -> (
    match a with
    | Void        -> (tupleListMaker b)
    | Nil         -> (tupleListMaker b)
    | Bool(true)  -> (tupleListMaker b)
    | Bool(false) -> (tupleListMaker b)
    | any         -> ( List.append (tupleMaker a) (tupleListMaker b) )
                     )

and tupleMaker sexpr = (* TODO: which strings should be inputted with each type? *)
  match sexpr with
  | Number(Int(vali))           -> (sexpr, (num_char_size, "MAKE_INT("^ (string_of_int vali) ^")"))
  | Number(Float(vali))         -> (sexpr, (num_char_size, "MAKE_FLOAT("^ (string_of_float vali) ^")"))
  | Char(vali)                  -> (sexpr, (num_char_size, "MAKE_CHAR("^  (Char.escaped vali) ^")"))
  | String(str)                 -> (let len = (length str) in (sexpr, ((type_size + len), "MAKE_STRING("^str^")")))
  | Symbol(str)                 -> (let len = (length str) in (sexpr, ((type_size + len), "MAKE_SYMBOL("^str^")")))
  | Pair(a,b)                   -> ()
  | TaggedSexpr 
  | TagRef 
  | any                         -> raise X_syntax_error

(*TEST:  offsetFixer [("a",(3,"a")); ("b",(4,"f")); ("c",(8,"f"))];;   *)
and offsetFixer lis = 
  let byteCounter = ref 6 in (* we start with 6 because of the basics table. *)
  let fixerFunc countVal (sexpr, (oldVal, string))  =
    let countVal = !byteCounter in 
    let byteCounterInc() = byteCounter := !byteCounter + oldVal in
    begin 
    byteCounterInc() ;
    (sexpr, (countVal, string))
    end in
  let fixer = (fixerFunc (!byteCounter)) in
  ( List.map fixer lis )

and constScanner exp = 
  match exp with
  | Const'(sexpr)                             -> [sexpr]
  | Seq'(listOfexprs)                         -> (seqConstScanHelper listOfexprs)
  | If'(test,dit,dif)                         -> (seqConstScanHelper [test;dit;dif] )
  | Or'(listOfexprs)                          -> (seqConstScanHelper listOfexprs)
  | Var'(var)                                 -> []
  | Box'(name)                                -> []
  | BoxGet'(name)                             -> []
  | BoxSet'(name,valu)                        -> constScanner valu
  | Set'(vari, valu)                          -> constScanner valu (*   set is expr*expr, but we look at it as a var*expr   *)
  | Def'(head, valu)                          -> constScanner valu (*   def is expr*expr, but we look at it as a var*expr   *)
  | LambdaSimple'(lambdaParams, bodyOfLambda) -> constScanner bodyOfLambda
  | LambdaOpt'(lambdaParams,vs, bodyOfLambda) -> constScanner bodyOfLambda
  | Applic'(rator, rands)                     -> ( List.append (constScanner rator) (seqConstScanHelper rands) )
  | ApplicTP'(rator, rands)                   -> ( List.append (constScanner rator) (seqConstScanHelper rands) )

  
and seqConstScanHelper listOfExprs = 
    match listOfExprs with
    | []     -> []
    | a :: b -> ( List.append (constScanner a) (seqConstScanHelper b) )

;;


  let make_consts_tbl asts = raise X_not_yet_implemented (* const_table_maker asts  *);;
  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;

  end;;

  open PC;;
  open Reader;;
  open Tag_Parser;;
  open Semantics;;

 
 
  (* TEST AREA
  
   make_consts_tbl
    (run_semantics
     (tag_parse_expression
      (read_sexpr
        "((define x 2) (define y 3))"
      )
     )
    );;

  run_semantics (
    Applic (Def (Var "x", Const (Sexpr (Number (Int 2)))),
   [Def (Var "y", Const (Sexpr (Number (Int 3))))])
   );;  

  *) 


  
