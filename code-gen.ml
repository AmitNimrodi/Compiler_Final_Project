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
let pair_size = (2*word_size + type_size) ;;
let tagCounter = ref 1 ;;
let tagCounterInc x = tagCounter := !tagCounter + x;;
let getAndIncTagCounter x = 
  let valu = !tagCounter in
  (tagCounterInc x);
  valu;;
let byteCounter = ref 6 ;;    (* we start with 6 because of the basics table. *)
let byteCounterInc x = byteCounter := !byteCounter + x ;;
let getAndInc x =
  let valu = !byteCounter in
  (byteCounterInc x); 
  valu;;

  let constsEqualizer e1 e2 =
    match e1, e2 with
      | Void, Void -> true
      | Void, Sexpr s2 -> false
      | Sexpr s1, Void -> false
      | Sexpr s1, Sexpr s2 -> sexpr_eq s1 s2
      ;;  



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
  let fixedTagsList        = ( tagFixConstList constSexprsList ) in
  let dupelessConstList    = ( cleanDupes fixedTagsList ) in
  let extendedList         = ( extendList dupelessConstList) in
  let dupelessExtendedList = ( cleanDupes extendedList ) in
  let basicList            = [ (Void, (0, "MAKE_VOID"));                  (Sexpr(Nil), (1, "MAKE_NIL"));
                               (Sexpr(Bool(false)), (2, "MAKE_BOOL(0)")); (Sexpr(Bool(true)), (4, "MAKE_BOOL(1)")) ] in 
  let tupledList           = ( tupleListMaker dupelessExtendedList basicList ) in
  tupledList

(*TEST:

[Sexpr (TaggedSexpr ("a", Number (Int 2)));
 Sexpr (Pair (TaggedSexpr ("a", Bool true), Pair (TagRef "a", Nil)))]
*)
and tagFixConstList constSexprsList = 
  match constSexprsList with
  | [] -> []
  | a :: b -> (
    match a with 
    | Sexpr(something) -> ( List.append [(fixThisTag something (string_of_int (getAndIncTagCounter 1)))] (tagFixConstList b) )
    | Void ->  (tagFixConstList b) 
    
  )

and fixThisTag a fixer=
  match a with
  | Pair(first,second)  -> (pairTagFixer first second fixer) 
  | TaggedSexpr(x,valu) -> (Sexpr(TaggedSexpr((x^fixer), valu)))
  | TagRef(x)           -> (Sexpr(TagRef(x^fixer)))
  | any                 -> (Sexpr(a))

and pairTagFixer first second fixer = 
  let fixedFirst = (fixThisTag first fixer) in
  let fixedSecond = (fixThisTag second fixer) in
  let fixFirst = (
    match fixedFirst with
    | Sexpr(something) -> something
    | any -> raise X_syntax_error
  ) in 
  let fixSecond = (
    match fixedSecond with
    | Sexpr(something) -> something
    | any -> raise X_syntax_error
  ) in
  (Sexpr(Pair(fixFirst,fixSecond)))

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
    | Void               -> ( extendList b )
    
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
  | []                    -> tuplesList
  | a :: b                -> (
    match a with
    | Void                -> (tupleListMaker b tuplesList)
    | Sexpr(Nil)          -> (tupleListMaker b tuplesList)
    | Sexpr(Bool(true))   -> (tupleListMaker b tuplesList)
    | Sexpr(Bool(false))  -> (tupleListMaker b tuplesList)
    | any                 -> (let lis = (List.append tuplesList [(tupleMaker a tuplesList)]) in
                             (tupleListMaker b lis)
                             )
                            )

and tupleMaker sexpr tuplesList = (* TODO: which strings should be inputted with each type? *)
  match sexpr with
  | Sexpr(Number(Int(valu)))           -> let offset = getAndInc(num_char_size) in 
                                          (sexpr, (offset, "MAKE_LITERAL_INT(" ^ (string_of_int valu) ^")"))
  | Sexpr(Number(Float(valu)))         -> let offset = getAndInc(num_char_size) in 
                                          (sexpr, (offset, "MAKE_LITERAL_FLOAT(" ^ (string_of_float valu) ^")"))
  | Sexpr(Char(valu))                  -> let offset = getAndInc(num_char_size) in 
                                          (sexpr, (offset, "MAKE_LITERAL_CHAR(" ^ (Char.escaped valu) ^")"))
  | Sexpr(String(str))                 -> let len = (String.length str) in
                                          let offset = getAndInc((len + num_char_size)) in
                                          (sexpr, (offset, "MAKE_LITERAL_STRING " ^ str))
  | Sexpr(Symbol(str))                 -> let offset = getAndInc(num_char_size) in
                                          (sexpr, (offset, "MAKE_LITERAL_SYMBOL(const_tbl+" ^ (symbolTupleMaker sexpr tuplesList) ^ ")"))
  | Sexpr(Pair(first,second))          -> (pairTupleMaker first second tuplesList)
  | Sexpr(TaggedSexpr(a,b))            -> raise X_syntax_error
  | Sexpr(TagRef(a))                   -> raise X_syntax_error
  | any                                -> (sexpr, (555, "AMIT")) (*TO CHECK: Nil/Void/Boolean might get here from a recursive call on a pair *)


and pairTupleMaker first second tuplesList = 
  let pred vari sexprTuple = 
    (match sexprTuple with
    | (Sexpr(x), (off, representation)) -> (constsEqualizer (Sexpr(x)) (Sexpr(vari)))
    | (Void, (off, representation))     -> (constsEqualizer Void (Sexpr(vari)))
    ) in
  let offsetPointer vari =
    ( match (List.find (pred vari) tuplesList) with
      | (x, (off, representation)) -> off
    ) in
  let offsetA = (offsetPointer first) in
  let offsetB = (offsetPointer second) in
  let valu = getAndInc(pair_size) in
  (Sexpr(Pair(first,second)), (valu, "MAKE_LITERAL_PAIR(const_tbl+"^(string_of_int offsetA)^ ", const_tbl+"^(string_of_int offsetB) ^")"))


  (* TEST :  symbolTupleMaker (Sexpr(Symbol("aaa"))) [(Sexpr(String("aa")), (6,"something"));  (Sexpr(String("aaa")), (17, "something"))];;
     EXPECTED OUTPUT: (Symbol "aaa", (23, "MAKE_SYMBOL(17)")) *)
and symbolTupleMaker sexpr tuplesList = 
  let str = ( match sexpr with
    | Sexpr(Symbol(stri)) -> stri
    | any -> raise X_syntax_error ) in
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
  (string_of_int offsetPointer)
  

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

(* test:
(Def' (Var' (VarFree "y"),
 Const' (Sexpr (Pair (TaggedSexpr ("a", Bool true), Pair (TagRef "a", Nil))))))
  *)
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

let rec address_in_const_table to_find consts =
  match consts with
  | [] -> raise X_syntax_error
  | (elem , (address , str)) :: resConsts -> (
        if (constsEqualizer to_find elem)
        then address
        else (address_in_const_table to_find resConsts) )
  ;;

let rec address_in_fvar_table to_find fvars =
  match fvars with
  | [] -> raise X_syntax_error
  | (str,address) :: resFvars -> (
        if (str=to_find)
        then address
        else (address_in_fvar_table to_find resFvars) )
  ;;


let rec code_gen_maker consts fvars e =
  let somthing = (code_genScanner consts fvars e 0 ) in
  somthing
  (* "hello: Yoav Is King" *)



and code_genScanner consts fvars exp envLayer = 
  match exp with
  | Const'(sexpr)                             -> 
            (const_genHelper consts fvars sexpr envLayer)
  | Var'(VarParam(name,mino))                 -> 
            (varParam_genHelper consts fvars mino envLayer)
  | Var'(VarBound(name,majo,mino))            -> 
            (varBound_genHelper consts fvars majo mino envLayer)
  | Var'(VarFree(name))                       -> 
            (varFree_genHelper consts fvars name envLayer)
  | Set'(Var'(VarParam(name,mino)), valu)     -> 
            (setvarParam_genHelper consts fvars mino valu envLayer)
  | Set'(Var'(VarBound(name,majo,mino)), valu)->
            (setvarBound_genHelper consts fvars majo mino valu envLayer)
  | Set'(Var'(VarFree(name)), valu)           -> 
            (setvarFree_genHelper consts fvars name valu envLayer)
  | Seq'(listOfexprs)                         -> 
            (seq_genHelper consts fvars listOfexprs envLayer)
  | Or'(listOfexprs)                          -> 
            (or_genHelper consts fvars listOfexprs envLayer)
  | If'(test,dit,dif)                         -> 
            (if_genHelper consts fvars test dit dif envLayer)
  | BoxGet'(head)                             -> 
            (boxget_genHelper consts fvars head envLayer)
  | BoxSet'(head,valu)                        -> 
            (boxset_genHelper consts fvars head valu envLayer)
  | LambdaSimple'(lambdaParams, bodyOfLambda) -> 
            (simple_genHelper consts fvars bodyOfLambda envLayer)
  | Applic'(rator, rands)                     -> 
            (applic_genHelper consts fvars rator rands envLayer)
  | LambdaOpt'(lambdaParams, vs, bodyOfLambda)-> 
            (opt_genHelper consts fvars lambdaParams vs bodyOfLambda envLayer)
  | ApplicTP'(rator, rands)                   ->
            (applicTP_genHelper consts fvars rator rands envLayer)
  
  
  | Def'(head, valu)                          -> raise X_syntax_error
  | Box'(name)                                -> raise X_syntax_error
  | any                                       -> raise X_syntax_error
  


and const_genHelper consts fvars sexpr envLayer =
  let adrs = (address_in_const_table sexpr consts) in
  (* "    mov rax, SOB_FALSE_ADDRESS"^ "\n"  *)
  "   mov rax, const_tbl+" ^ (string_of_int adrs) ^            " \n" 


and varParam_genHelper consts fvars mino envLayer =
  "   mov rax, qword [rbp +8 * (4 +" ^(string_of_int mino) ^ ")] \n"


and varBound_genHelper consts fvars majo mino envLayer =
  let major = (string_of_int majo) in
  let minor = (string_of_int mino) in   
  "   mov rax, qword [rbp + 8 ∗ 2]  "                        ^ " \n" ^
  "   mov rax, qword [rax + 8 ∗ "  ^ major ^ "]"             ^ " \n" ^
  "   mov rax, qword [rax + 8 ∗ " ^ minor ^ "]"              ^ " \n"   
  

and varFree_genHelper consts fvars name envLayer =
  let address = "fvar_tbl + " ^ (string_of_int (address_in_fvar_table name fvars)) in
  "   mov rax, qword [" ^ address ^ "]"                      ^ " \n"  

  
and setvarParam_genHelper consts fvars mino valu envLayer =
  let minor = (string_of_int mino) in  
  (code_genScanner consts fvars valu envLayer)               ^ " \n" ^
  "   mov qword [rbp + 8 ∗ (4 + "  ^ minor ^ ")], rax"       ^ " \n" ^   (*rax holds the value of evaluated VALU parameter *)
  "   mov rax, SOB_VOID_ADDRESS"                             ^ " \n"   


and setvarBound_genHelper consts fvars majo mino valu envLayer =
  let major = (string_of_int majo) in
  let minor = (string_of_int mino) in   
  (code_genScanner consts fvars valu envLayer)               ^ " \n" ^
  "   mov rbx, qword [rbp + 8 ∗ 2]"                          ^ " \n" ^
  "   mov rbx, qword [rbx + 8 ∗"  ^ major ^ "]"              ^ " \n" ^   (*rax holds the value of evaluated VALU parameter *)
  "   mov qword [rbx + 8 ∗" ^ minor ^ "], rax"               ^ " \n" ^
  "   mov rax, SOB_VOID_ADDRESS"                             ^ " \n"  


and setvarFree_genHelper consts fvars name valu envLayer =
  let address = "fvar_tbl + " ^ (string_of_int (address_in_fvar_table name fvars)) in
  (code_genScanner consts fvars valu envLayer)              ^ " \n" ^
    "   mov qword [ " ^ address ^ "], rax"                  ^ " \n" ^
    "   mov rax, SOB_VOID_ADDRESS"                          ^ " \n"  

and seq_genHelper consts fvars listOfexprs envLayer =
  raise X_syntax_error


and or_genHelper consts fvars listOfexprs envLayer =
  raise X_syntax_error



(* 
    First, we get uniqe label, then we concat assembly string:
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
  let ifLable = labelCounterGet() in
  (
  (code_genScanner consts fvars test envLayer)  ^ " \n" ^
  "   cmp rax, SOB_FALSE_ADDRESS"                       ^ " \n" ^
  "   je  Lelse" ^ (string_of_int ifLable)      ^ " \n" ^
  (code_genScanner consts fvars dit envLayer)   ^ " \n" ^
  "   jmp  Lexit" ^ (string_of_int ifLable)     ^ " \n" ^
  "Lelse" ^ (string_of_int ifLable) ^ ":"       ^ " \n" ^
  (code_genScanner consts fvars dif envLayer)   ^ " \n" ^
  "Lexit" ^ (string_of_int ifLable) ^ ":"       ^ " \n" 
  )
  

and boxget_genHelper consts fvars head envLayer =
  raise X_syntax_error


and boxset_genHelper consts fvars head valu envLayer =
  raise X_syntax_error



(* 
    First, we get uniqe label, then we concat assembly string:
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
and simple_genHelper consts fvars bodyOfLambda envLayer =
  labelCounterInc();
  let sLabel = labelCounterGet() in
  (

  "   mov r10, " ^ (string_of_int(envLayer+1))  ^ " \n" ^
  "   shl r10, 3"                               ^ " \n" ^
  "   MALLOC r10, r10"                          ^ " \n" ^(* ExtEnv malloced  *)
  
  
  (* 
    for( r11=i=0 , r12=j=1 ; r11 < r13=envLayer ; r11++ , r12++)
    ( ExtEnv[r12=j] = Env[r11=i])
  *)
  
  "   mov r11, 0"                               ^ " \n" ^(* i=0  *)
  "   mov r12, 1"                               ^ " \n" ^(* j=1  *)
  "   mov r13, " ^ (string_of_int envLayer)     ^ " \n" ^(* r13=envLayer  *)

  "ExtEnvLoop"   ^ (string_of_int sLabel) ^ ":" ^ " \n" ^
  "   cmp r11, r13"                             ^ " \n" ^(* i=j?  *)
  "   je ExtEnvEnd" ^ (string_of_int sLabel)    ^ " \n" ^
  
  
  "   mov r14, qword[rbp + 8*2]"                ^ " \n" ^(* Env in stack  *)
  "   mov r8, r10"                              ^ " \n" ^(* ExtEnv malloced  *)
  
  "   mov r15, r11"                             ^ " \n" ^(* r15=i  *)
  "   shl r15, 3"                               ^ " \n" ^(* r15=8*i  *)
  "   add r14, r15"                             ^ " \n" ^(* r14=Env[i]  *)
  
  "   mov r15, r12"                             ^ " \n" ^(* r15=j  *)
  "   shl r15, 3"                               ^ " \n" ^(* r15=8*j  *)
  "   add r8, r15"                              ^ " \n" ^(* r8=ExtEnv[j]  *)
  

  "   mov r15, qword[r14]"                      ^ " \n" ^(* r15=[Env[i]]  *)
  "   mov qword[r8], r15"                       ^ " \n" ^(* [ExtEnv[j]]=r15  *)


  "   add r11, 1"                               ^ " \n" ^(* i++  *)
  "   add r12, 1"                               ^ " \n" ^(* j++  *)
  "   jmp ExtEnvLoop" ^ (string_of_int sLabel)  ^ " \n" ^
  "ExtEnvEnd"    ^ (string_of_int sLabel) ^ ":" ^ " \n" ^

  "   mov r11, 0"                               ^ " \n" ^(* i=0  *)
  "   mov r13, qword[rbp + 8*3]"                ^ " \n" ^(* r13=paramcount  *)
  "   mov r9, qword[rbp + 8*3]"                 ^ " \n" ^
  "   shl r9, 3"                                ^ " \n" ^
  "   MALLOC r9, r9"                            ^ " \n" ^(* param vector malloced  *)
  "   mov qword[r10], r9"                       ^ " \n" ^(* [ExtEnv[0]]=r9  *)
  
  (* 
    for( r11=i=0 ; r11 < r13=paramcount ; r11++ )
    ( ExtEnv[r12=j] = Env[r11=i])
  *)
  
  "ParamEnvLoop" ^ (string_of_int sLabel) ^ ":" ^ " \n" ^
  "   cmp r11, r13"                             ^ " \n" ^(* i=paramcount?  *)
  "   je ParamEnvEnd" ^ (string_of_int sLabel)  ^ " \n" ^
  
  "   mov r15, r11"                             ^ " \n" ^(* r15=i  *)
  "   add r15, 4"                               ^ " \n" ^(* r15=i+4  *)
  "   shl r15, 3"                               ^ " \n" ^(* r15=8*(i+4)  *)
  "   add r15, rbp"                             ^ " \n" ^(* r15=8*(i+4)+rbp=param[i]  *)
  
  "   mov r14, r11"                             ^ " \n" ^(* r14=i  *)
  "   shl r14, 3"                               ^ " \n" ^(* r14=8*i  *)
  "   add r14, r9"                              ^ " \n" ^(* r15=8*i+param vector  *)
  
  "   mov qword[r14], r15"                      ^ " \n" ^(* [vector[i]]=r15  *)


  "   add r11, 1"                               ^ " \n" ^(* i++  *)
  "   jmp ParamEnvLoop" ^(string_of_int sLabel) ^ " \n" ^
  "ParamEnvEnd"  ^ (string_of_int sLabel) ^ ":" ^ " \n" ^
  
  (* 
    allocate closure object, adress in rax.
    set closure env and code(second and third parameters to make_closure)
    jump to continue.
  *)

  "   mov rax, r10"                             ^ " \n" ^(* rax=ExtEnv  *)
  "   MAKE_CLOSURE( rax, r10,Lcode" 
                 ^(string_of_int sLabel) ^ ")"  ^ " \n" ^(* rax=closure  *)
  "   jmp Lcont" ^(string_of_int sLabel)        ^ " \n" ^

  
  "Lcode"        ^(string_of_int sLabel) ^ ":"  ^ " \n" ^
  "   push rbp"                                 ^ " \n" ^(* save pointer  *)
  "   mov rbp, rsp"                             ^ " \n" ^(* point to new closure  *)
  (code_genScanner consts fvars bodyOfLambda (envLayer+1))
                                                ^ " \n" ^
  "   leave"                                    ^ " \n" ^(* rax=ExtEnv  *)
  "   ret"                                      ^ " \n" ^(* rax=ExtEnv  *)
  
  "Lcont"        ^(string_of_int sLabel) ^ ":"  ^ " \n" 

  
  ) 


and applic_genLoper consts fvars rands envLayer =
  match rands with
  | [] -> ""
  | a :: b -> 
      (code_genScanner consts fvars a envLayer) ^ " \n" ^(* eval current rand *)
      "   push rax"                             ^ " \n" ^(* save answer  *)
      (applic_genLoper consts fvars b envLayer)  

  

and applic_genHelper consts fvars rator rands envLayer =
  labelCounterInc();
  let aLabel = labelCounterGet() in
  let nRands = (List.length rands) in
  let randsLoper = (applic_genLoper consts fvars (List.rev rands) envLayer) in
  
  (

  (randsLoper)                                  ^ " \n" ^(* eval rands and push *)
  "   mov r10, " ^(string_of_int nRands)        ^ " \n" ^(* r10 =nRands  *)
  "   push r10"                                 ^ " \n" ^(* push nRands  *)
  (code_genScanner consts fvars rator envLayer) ^ " \n" ^(* eval rator *)
  "   mov sil, byte[rax]"                       ^ " \n" ^(* check if rator is closure*)
  "   cmp sil, T_CLOSURE"                       ^ " \n" ^(* check if rator is closure*)
  "   jne ApplicError" ^ (string_of_int aLabel)  ^ " \n" ^(* error if no closure *)
  
  "   CLOSURE_ENV r9, rax"                      ^ " \n" ^(* r9 = Env  *)
  "   push r9"                                  ^ " \n" ^(* push closure Env*)
  "   CLOSURE_CODE r8, rax"                     ^ " \n" ^(* r9 = code  *)
  "   call r8"                                  ^ " \n" ^(* call closure code*)
  (*
  upon return, pop env and args,
  we notice args can have different length after call so we
  take back nRands, and pop env iff rator is of type closure
  *)
  "   add rsp, 8*1"                             ^ " \n" ^(* pop Env *)
  
  "ApplicError"  ^(string_of_int aLabel) ^ ":"  ^ " \n" ^
  "   pop rbx"                                  ^ " \n" ^(* pop nRands count *)
  "   shl rbx, 3"                               ^ " \n" ^(* nRands*8 *)
  "   add rsp, rbx"                             ^ " \n" (* pop nRands *)
  
  )


  
and opt_genHelper consts fvars lambdaParams vs bodyOfLambda envLayer =
  raise X_syntax_error


  
and applicTP_genHelper consts fvars rator rands envLayer =
  labelCounterInc();
  let aTPLabel = labelCounterGet() in
  let nTPRands = (List.length rands) in
  let randsTPLoper = (applic_genLoper consts fvars (List.rev rands) envLayer) in
  
  (

  (randsTPLoper)                                ^ " \n" ^(* eval rands and push *)
  "   mov r10, " ^(string_of_int nTPRands)      ^ " \n" ^(* r10 =nTPRands  *)
  "   push r10"                                 ^ " \n" ^(* push nTPRands  *)
  (code_genScanner consts fvars rator envLayer) ^ " \n" ^(* eval rator *)
  "   cmp byte[rax], T_CLOSURE"                 ^ " \n" ^(* check if rator is closure*)
  "   je ApplicError" ^ (string_of_int aTPLabel)^ " \n" ^(* error if no closure *)
  
  "   CLOSURE_ENV r9, rax"                      ^ " \n" ^(* r9 = Env  *)
  "   push r9"                                  ^ " \n" ^(* push closure Env*)
  
  (*
  push rbp+8 and move pointers to prepare loop
  *)
  "   mov r10, qword[rbp+ 8*1 ]"                ^ " \n" ^(* r10 =oldRetAdress  *)
  "   push r10"                                 ^ " \n" ^(* push oldRetAdress  *)
  
  "   mov r15, qword[rbp+ 8*3 ]"                ^ " \n" ^(* r15 =nTPRands  *)
  "   add r15, 4"                               ^ " \n" ^(* r15 =nTPRands+4  *)
  "   shl r15, 3"                               ^ " \n" ^(* r15 =(nTPRands+4)*8  *)
  
  "   mov r11, r15"                             ^ " \n" ^(* r11 =(nTPRands+4)*8  *)
  "   add r11, rbp"                             ^ " \n" ^(* r11 =(nTPRands+4)*8+rbp  *)
  
  
  "   mov r14, qword[rsp+ 8*1 ]"                ^ " \n" ^(* r14 =nTPRands  *)
  "   add r14, 2"                               ^ " \n" ^(* r14 =nTPRands+2  *)
  "   shl r14, 3"                               ^ " \n" ^(* r14 =(nTPRands+2)*8  *)
  
  "   mov r12, r14"                             ^ " \n" ^(* r12 =(nTPRands+2)*8  *)
  "   add r12, rsp"                             ^ " \n" ^(* r10 =(nTPRands+2)*8+rsp  *)
  
  "   mov r13, qword[rbp]"                      ^ " \n" ^(* save old rbp  *)
  
  (*
  loop with pointers to fix stack
  *)

  "ApplicTPLoop" ^(string_of_int aTPLabel) ^ ":"^ " \n" ^
  "   cmp r12, rsp"                             ^ " \n" ^(* r10 =(nTPRands+2)*8+rsp  *)
  "   je ApplicTPEnd" ^ (string_of_int aTPLabel)^ " \n" ^(* error if no closure *)
  "   mov r10, qword[r12]"                      ^ " \n" ^(* r10 =oldRetAdress  *)
  "   mov qword[r11], r10"                      ^ " \n" ^(* r10 =oldRetAdress  *)
  "   sub r12, 8"                               ^ " \n" ^(* r10 =oldRetAdress  *)
  "   sub r11, 8"                               ^ " \n" ^(* r10 =oldRetAdress  *)
  "   jmp ApplicTPLoop"^(string_of_int aTPLabel)^ " \n" ^(* error if no closure *)
  
  "ApplicTPEnd" ^(string_of_int aTPLabel) ^ ":" ^ " \n" ^
  "   mov r10, qword[r12]"                      ^ " \n" ^(* r10 =oldRetAdress  *)
  "   mov qword[r11], r10"                      ^ " \n" ^(* r10 =oldRetAdress  *)
  "   mov rbp, r13"                             ^ " \n" ^(* r10 =oldRetAdress  *)
  "   mov rsp, r11"                             ^ " \n" ^(* r10 =oldRetAdress  *)
  
  
  "   CLOSURE_CODE r8, rax"                     ^ " \n" ^(* r9 = code  *)
  "   call r8"                                  ^ " \n" ^(* call closure code*)
  (*
  upon return, pop env and args,
  we notice args can have different length after call so we
  take back nTPRands, and pop env iff rator is of type closure
  *)
  "   add rsp, 8*1"                             ^ " \n" ^(* pop Env *)
  
  "ApplicError"  ^(string_of_int aTPLabel) ^ ":"^ " \n" ^
  "   pop rbx"                                  ^ " \n" ^(* pop nTPRands count *)
  "   shl rbx, 3"                               ^ " \n" ^(* nTPRands*8 *)
  "   add rsp, rbx"                             ^ " \n" (* pop nTPRands *)
  
  )


  
    
;;

  let make_consts_tbl asts = const_table_maker asts ;;
  let make_fvars_tbl asts = free_table_maker asts ;;
  let generate consts fvars e = code_gen_maker consts fvars e ;;

  end;;

  open PC;;
  open Reader;;
  open Tag_Parser;;
  open Semantics;;
  open Code_Gen;;


  (* TEST AREA
  
   let yoav1 = 
    make_consts_tbl
    [(run_semantics
     (tag_parse_expression
      (read_sexpr
        "(if 1 2 3)"
      )
     )
    )];;

    let yoav2 = 
    make_fvars_tbl
    [(run_semantics
     (tag_parse_expression
      (read_sexpr
        "(if 1 2 3)"
      )
     )
    )];;
    let yoav3 = 
    make_consts_tbl
      [(run_semantics
     (tag_parse_expression
      (read_sexpr
        "(define x '(a b 1 2 3 4 1 2))"
      )
     )
    )];;

      make_consts_tbl
      [(run_semantics
     (tag_parse_expression
      (read_sexpr
        "'(a)"
            )
          )
        )];;

        make_consts_tbl
      [(run_semantics
     (tag_parse_expression
      (read_sexpr
        "'a"
            )
          )
        )];;

    "(if 1 2 3)"
  
  Applic' (Def' (Var' (VarFree "x"), Const' (Sexpr (Number (Int 2)))),
    [Def' (Var' (VarFree "y"), Const' (Sexpr (Number (Int 3))));
     Def' (Var' (VarFree "z"), Const' (Sexpr (Number (Int 4))))])

 make_consts_tbl [
  Applic' (Def' (Var' (VarFree "x"), Const' (Sexpr (Number (Int 2)))),
    [Def' (Var' (VarFree "y"), Const' (Sexpr (Number (Int 3))));
     Def' (Var' (VarFree "z"), Const' (Sexpr (Number (Int 4))))])]
  

  run_semantics (
    Applic (Def (Var "x", Const (Sexpr (Number (Int 2)))),
   [Def (Var "y", Const (Sexpr (Number (Int 3))))])
   );;  

   tupleListMaker [Sexpr (String "a"); Sexpr (Symbol "a")] basicList;;


  *) 


  
