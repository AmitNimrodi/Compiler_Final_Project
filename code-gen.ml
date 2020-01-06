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








  
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  (*       HERE WE ARE STARTING WITH THE ASSIGNMENT!!!    *)
  






let type_size = 1 ;;
let word_size = 8 ;;
let num_char_size = (type_size + word_size) ;;
let byteCounter = ref 6 ;;    (* we start with 6 because of the basics table. *)
let byteCounterInc x = byteCounter := !byteCounter + x ;;
let getAndInc x =
  let valu = !byteCounter in
  (byteCounterInc x); 
  valu;;

(* TEST:  cleanDupes [1;1;1;2;3;4;5;4;3;3;6;2;2]  
TODO: might not know how to equalise objects - use expr'_eq instead *)
let rec cleanDupes sexprList =
  let a = cleanDupesinner (List.rev sexprList) in
    (List.rev a)

and cleanDupesinner sexprList = 
  match sexprList with
  | []      -> []
  | a :: b  -> (
    match (List.mem a b) with
    | true  -> ( cleanDupesinner b )
    | false -> ( List.append [a] (cleanDupesinner b) )
    )
;;

let rec const_table_maker listOfExprs = 
  
  let constSexprsList      = ( findConsts listOfExprs ) in
  let dupelessConstList    = ( cleanDupes constSexprsList ) in
  let extendedList         = ( extendList dupelessConstList) in
  let dupelessExtendedList = ( cleanDupes extendedList ) in
  let basicList            = [ (Void, (0, "MAKE_VOID"));                  (Sexpr(Nil), (1, "MAKE_NIL"));
                            (Sexpr(Bool(false)), (2, "MAKE_BOOL(0)")); (Sexpr(Bool(true)), (4, "MAKE_BOOL(1)")) ] in 
  let tupledList           = ( tupleListMaker dupelessExtendedList basicList ) in
  (*
  let offsetFixedList      = offsetFixer  ( List.append basicList tupledList ) in
  offsetFixedList
  *)
  tupledList

and findConsts listOfExprs = 
match listOfExprs with
  | []     -> []
  | a :: b -> ( List.append (constScanner a) (findConsts b) ) 
   
   (* input: [Sexpr(Pair(Number(Int(1)), Pair(Number(Int(2)), Nil))); Sexpr(Symbol("ab"))]
      output: [Sexpr(Number(Int(1))); Sexpr(Number(Int(2))); Sexpr(Pair(Number(Int(2)), Nil));
             Sexpr(Pair(Number(Int(1)), Pair(Number(Int(2)), Nil))); Sexpr(String("ab")); Sexpr(Symbol("ab"))]    *)
            
(*TESTED WITH INPUT ABOVE - WORKS *) (* should each match case be wrapped with Sexpr'(something) ? *)
and extendList sexprList = 
  match sexprList with
  | [] -> []
  | a :: b -> (
    match a with
    | Sexpr(Symbol(str)) -> ( List.append [ Sexpr(String(str)); a ] (extendList b) )
    | Sexpr(Pair(x,y))   -> ( List.append (pairExtender (Pair(x,y))) (extendList b) )
    | Sexpr(x)           -> ( List.append [ Sexpr(x) ] (extendList b) )
    | any                -> raise X_syntax_error
              )

(*TESTED WITH INPUT ABOVE - WORKS *)
and pairExtender expr = 
  match expr with
  | Pair(Nil,Nil) -> []
  | Pair(x, Nil)  -> ( List.append (extendList [ Sexpr(x) ] )  [ Sexpr(expr) ] )
  | Pair(x,y)     -> ( List.append
                     ( List.append (extendList [ Sexpr(x) ] ) (extendList [ Sexpr(y) ]) )
                      [ Sexpr(Pair(x,y)) ] )
  | any           -> raise X_syntax_error

and tupleListMaker sexprsList tuplesList = 
  match sexprsList with
  | []                    -> []
  | a :: b                -> (
    match a with
    | Void                -> (tupleListMaker b tuplesList)
    | Sexpr(Nil)          -> (tupleListMaker b tuplesList)
    | Sexpr(Bool(true))   -> (tupleListMaker b tuplesList)
    | Sexpr(Bool(false))  -> (tupleListMaker b tuplesList)
    | any                 -> (let lis = (List.append tuplesList [(tupleMaker a tuplesList)]) in
                             ( List.append lis (tupleListMaker b lis) )
                             )
                            )

and tupleMaker sexpr tuplesList = (* TODO: which strings should be inputted with each type? *)
  match sexpr with
  | Sexpr(Number(Int(valu)))           -> let offset = getAndInc(num_char_size) in 
                                          (sexpr, (offset, "MAKE_INT("^ (string_of_int valu) ^")"))
  | Sexpr(Number(Float(valu)))         -> let offset = getAndInc(num_char_size) in 
                                          (sexpr, (offset, "MAKE_FLOAT("^ (string_of_float valu) ^")"))
  | Sexpr(Char(valu))                  -> let offset = getAndInc(num_char_size) in 
                                          (sexpr, (offset, "MAKE_CHAR("^  (Char.escaped valu) ^")"))
  | Sexpr(String(str))                 -> let len = (String.length str) in
                                          let offset = getAndInc((type_size + len + num_char_size)) in
                                          (sexpr, (offset, "MAKE_STRING("^ str ^")"))
  | Sexpr(Symbol(str))                 -> (symbolTupleMaker sexpr tuplesList)
  | Sexpr(Pair(a,b))                   -> raise X_syntax_error
  | Sexpr(TaggedSexpr(a,b))            -> raise X_syntax_error
  | Sexpr(TagRef(a))                 -> raise X_syntax_error
  | any                                -> raise X_syntax_error


  (* TEST :  symbolTupleMaker (Symbol("aaa")) [(Sexpr(String("aa")), (6,"something"));  (Sexpr(String("aaa")), (17, "something"))];;
     EXPECTED OUTPUT: (Symbol "aaa", (23, "MAKE_SYMBOL(17)")) *)
and symbolTupleMaker sexpr tuplesList = 
  let str = ( match sexpr with
    | Sexpr(Symbol(stri)) -> stri
    | any -> raise X_syntax_error ) in
  let valu = getAndInc(type_size + num_char_size) in
  let pred sexprTuple = 
  ( match sexprTuple with
    | (Sexpr(String(strin)), (off, representation)) -> if (strin=str) then true else false
    | any -> false
  ) in
  let offsetPointer =
  ( match (List.find pred tuplesList) with
    | (Sexpr(String(strin)), (off, representation)) -> off
    |  any -> raise X_syntax_error
  ) in
  (sexpr, (valu, "MAKE_SYMBOL("^(string_of_int offsetPointer)^")"))

(*TEST:  offsetFixer [("a",(3,"a")); ("b",(4,"f")); ("c",(3,"f"))];;   *)
(*
and offsetFixer lis = 
  let fixerFunc countVal (sexpr, (oldVal, string))  =
    let countVal = !byteCounter in 
    
    begin 
    byteCounterInc() ;
    (sexpr, (countVal, string))
    end in
  let fixer = (fixerFunc (!byteCounter)) in
  ( List.map fixer lis )
*)

and constScanner exp = (* should each match case be wrapped with Expr'(intervalValue) ? *)
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




  
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE CONST_TABLE!!!    *)
  

let fixed_free_labels = 
  ["boolean?"; "float?"; "integer?"; "pair?"; "null?"; "char?";
   "string?"; "procedure?"; "symbol?"; "string-length";
   "string-ref"; "string-set!"; "make-string"; "symbol->string"; 
   "char->integer"; "integer->char"; "eq?"; 
   "+"; "*"; "-"; "/"; "<"; "="
(* you can add yours here *)];;




let rec free_table_maker listOfExprs = 
  let freeSexprsList      = ( findFree listOfExprs ) in
  let fixedFreeSexprsList = ( List.append fixed_free_labels freeSexprsList ) in
  let dupelessFreeList    = ( cleanDupes fixedFreeSexprsList ) in
  let tupledList          = ( tupleListMaker dupelessFreeList 0 ) in
  
  tupledList
  


and findFree listOfExprs = 
  match listOfExprs with
  | []     -> []
  | a :: b -> ( List.append (freeScanner a) (findFree b) ) 


and tupleListMaker listOfExprs n = 
  match listOfExprs with
  | []     -> []
  | a :: b -> ( List.append [(a,n)] (tupleListMaker b (n+1) ) ) 


and freeScanner exp = 
  match exp with
  | Var'(vari) -> (varScanner vari)

  | Const'(sexpr) -> []
  | Box'(name)    -> []
  | BoxGet'(name) -> []
  
  | BoxSet'(name,valu) -> 
      (freeScanner valu)
  | Set'(vari, valu) ->
      (freeScanner valu) (*   set is expr*expr, but we look at it as a var*expr   *)
  | Def'(head, valu) ->
      (freeScanner valu) (*   def is expr*expr, but we look at it as a var*expr   *)
  | LambdaSimple'(lambdaParams, bodyOfLambda) ->
      (freeScanner bodyOfLambda)
  | LambdaOpt'(lambdaParams,vs, bodyOfLambda) ->
      (freeScanner bodyOfLambda)
  
  | Applic'(rator, rands) ->
      ( List.append (freeScanner rator) (seqFreeScanHelper rands) )
  | ApplicTP'(rator, rands) ->
      ( List.append (freeScanner rator) (seqFreeScanHelper rands) )
  | Seq'(listOfexprs) -> 
      (seqFreeScanHelper listOfexprs)
  | If'(test,dit,dif) ->
      (seqFreeScanHelper [test;dit;dif] )
  | Or'(listOfexprs)  ->
      (seqFreeScanHelper listOfexprs)
  

 
and seqFreeScanHelper listOfExprs = 
  match listOfExprs with
  | []     -> []
  | a :: b -> ( List.append (freeScanner a) (seqFreeScanHelper b) )

  
and varScanner vari = 
  match vari with
  | VarBound(name,maj,mino) -> []
  | VarParam(name,mino) -> []
  | VarFree(name) -> [name]

  
;;
  
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  (*       HERE WE ARE FINISHED WITH THE FREE_TABLE!!!    *)
  
let labelCounter = ref 0 ;;    
let labelCounterInc() = labelCounter := !labelCounter + 1 ;;
let labelCounterGet() = !labelCounter;;



let rec code_gen_maker consts fvars e =
  let somthing = (code_genScanner consts fvars e 0 ) in
  "hello: Yoav Is King"



and code_genScanner consts fvars exp envLayer = 
  match exp with
  | Const'(sexpr)                             -> 
            (const_genHelper sexpr envLayer)
  | Var'(VarParam(name,mino))                 -> 
            (varParam_genHelper mino envLayer)
  | Var'(VarBound(name,majo,mino))            -> 
            (varBound_genHelper majo mino envLayer)
  | Var'(VarFree(name))                       -> 
            (varFree_genHelper name envLayer)
  | Set'(Var'(VarParam(name,mino)), valu)     -> 
            (setvarParam_genHelper mino valu envLayer)
  | Set'(Var'(VarBound(name,majo,mino)), valu)->
            (setvarBound_genHelper majo mino valu envLayer)
  | Set'(Var'(VarFree(name)), valu)           -> 
            (setvarFree_genHelper name valu envLayer)
  | Seq'(listOfexprs)                         -> 
            (seq_genHelper listOfexprs envLayer)
  | Or'(listOfexprs)                          -> 
            (or_genHelper listOfexprs envLayer)
  | If'(test,dit,dif)                         -> 
            (if_genHelper consts fvars test dit dif envLayer)
  | BoxGet'(head)                             -> 
            (boxget_genHelper head envLayer)
  | BoxSet'(head,valu)                        -> 
            (boxset_genHelper head valu envLayer)
  | LambdaSimple'(lambdaParams, bodyOfLambda) -> 
            (simple_genHelper consts fvars lambdaParams bodyOfLambda envLayer)
  | Applic'(rator, rands)                     -> 
            (applic_genHelper rator rands envLayer)
  | LambdaOpt'(lambdaParams, vs, bodyOfLambda)-> 
            (opt_genHelper lambdaParams vs bodyOfLambda envLayer)
  | ApplicTP'(rator, rands)                   ->
            (applicTP_genHelper rator rands envLayer)
  
  
  | Def'(head, valu)                          -> raise X_syntax_error
  | Box'(name)                                -> raise X_syntax_error
  | any                                       -> raise X_syntax_error
  


and const_genHelper sexpr envLayer =
  raise X_syntax_error


and varParam_genHelper mino envLayer =
  raise X_syntax_error


and varBound_genHelper majo mino envLayer =
  raise X_syntax_error


and varFree_genHelper name envLayer =
  raise X_syntax_error


and setvarParam_genHelper mino valu envLayer =
  raise X_syntax_error


and setvarBound_genHelper majo mino valu envLayer =
  raise X_syntax_error


and setvarFree_genHelper name valu envLayer =
  raise X_syntax_error


and seq_genHelper listOfexprs envLayer =
  raise X_syntax_error


and or_genHelper listOfexprs envLayer =
  raise X_syntax_error



(* 
    First, we get 2 uniqe labels, then we concat assembly string:
    eval test
    cmp to false to see if jmp to else_label
    eval dit
    jmp to exit_label
    else_label
    eval dif
    exit_label
*)
and if_genHelper consts fvars test dit dif envLayer =
  labelCounterInc();
  let ifLelse = labelCounterGet() in
  labelCounterInc();
  let ifLexit = labelCounterGet() in
  (
  (code_genScanner consts fvars test envLayer)  ^ " \n" ^
  "   cmp rax, SOB_FALSE"                       ^ " \n" ^
  "   je  Lelse" ^ (string_of_int ifLelse)      ^ " \n" ^
  (code_genScanner consts fvars dit envLayer)   ^ " \n" ^
  "   jmp  Lexit" ^ (string_of_int ifLexit)     ^ " \n" ^
  "Lelse" ^ (string_of_int ifLelse) ^ ":"       ^ " \n" ^
  (code_genScanner consts fvars dif envLayer)   ^ " \n" ^
  "Lexit" ^ (string_of_int ifLexit) ^ ":"       ^ " \n" 
  )
  

and boxget_genHelper head envLayer =
  raise X_syntax_error


and boxset_genHelper head valu envLayer =
  raise X_syntax_error



(* 
    First, we get 2 uniqe labels, then we concat assembly string:
    allocate ExtEnv using malloc
    copy pointers of minor vectors from Env to ExtEnv
    allocate new vector for ExtEnv(0) using malloc
    copy parameters from stack
    allocate closure object, adress in rax
    set rax.env = ExtEnv
    set rax.code = Lcode
    jmp Lcont
    Lcode: save rbp, eval body, return.
    Lcont: rest of code
*)
and simple_genHelper consts fvars lambdaParams bodyOfLambda envLayer =
  labelCounterInc();
  let Lcode = labelCounterGet() in
  labelCounterInc();
  let Lcont = labelCounterGet() in
  (

  "   mov r10, " ^ (string_of_int(envLayer+1))  ^ " \n" ^
  "   imul r10, 8"                              ^ " \n" ^
  "   MALLOC r10, r10"                          ^ " \n" ^
  
  
  )

and applic_genHelper rator rands envLayer =
  raise X_syntax_error


  
and opt_genHelper lambdaParams vs bodyOfLambda envLayer =
  raise X_syntax_error


  
and applicTP_genHelper rator rands envLayer =
  raise X_syntax_error



  
    
;;








  let make_consts_tbl asts = const_table_maker asts ;;
  let make_fvars_tbl asts = free_table_maker asts ;;
  let generate consts fvars e = code_gen_maker consts fvars e ;;

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


  
