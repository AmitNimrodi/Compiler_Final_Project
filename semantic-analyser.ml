#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

(* let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq var1 var2) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;; *)
	
                       
exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct

 





  let rec p_expr' e params bounds =
    let recCall x =  (p_expr' x params bounds) in
    match e with 
    | Const(sexpr) -> Const'(sexpr)
    | Var(name) -> (var'_expr'_helper name params bounds)
    | If(test,dit,dif) -> If'( recCall test, recCall dit, recCall dif )
    | Seq(listOfexprs) -> Seq'(List.map (function(elem)-> recCall elem) listOfexprs)
    | Set(vari, valu) -> Set'( recCall vari , recCall valu )
    | Def(head, valu) -> defineHelper (recCall head) (recCall valu)
    | Or(listOfexprs) -> Or'(List.map (function(elem)-> recCall elem) listOfexprs)
    | LambdaSimple(lambdaParams, body) ->
      LambdaSimple'(lambdaParams , (p_expr' body lambdaParams (List.append [params] bounds)))
    | LambdaOpt(lambdaParams,vs, body) -> 
      LambdaOpt'(lambdaParams , vs, (p_expr' body (List.append lambdaParams [vs] ) (List.append [params] bounds)))
    | Applic(rator, rands) ->
        Applic'( (recCall rator), (List.map (function(elem)-> recCall elem) rands) )
  
  
  and defineHelper head valu =
    match head with
    | Var'(VarFree(name)) -> Def'(head, valu)
    | any -> raise X_syntax_error
  
  
  and var'_expr'_helper nameOfVar params bounds =
    let (prm,loc) = (varCheckAndCount nameOfVar params 0) in
    let (bnd,major,minor) = (varCheckInBounds nameOfVar bounds 0) in
      match prm,bnd with
      | true,_ -> Var'(VarParam(nameOfVar,loc))
      | false,true -> Var'(VarBound(nameOfVar,major,minor))
      | false,false -> Var'(VarFree(nameOfVar))
  
  
  and varCheckInBounds e listOfLists majCounter = 
    match listOfLists with
    | [] -> (false, 0, 0)
    | a::b -> (let (ans, minor) = (varCheckAndCount e a 0) in
      if ans then (true, majCounter, minor)
      else (varCheckInBounds e b (majCounter+1)))
  
  and varCheckAndCount e listOfVars counter = 
    match listOfVars with
    | [] -> (false,0)
    | a::b -> if a = e then (true, counter)
      else (varCheckAndCount e b (counter+1))
  
  ;;
  
  
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE LEXICAL ADDRESSES!!!    *)
  
  
  let rec p_expr_tail' e t_p =
    match e with 
    | Const'(sexpr) -> e
    | Var'(name) -> e
    | Seq'(listOfexprs) -> Seq'(seq_tp_helper listOfexprs t_p)
    | Set'(vari, valu) -> Set'( vari , p_expr_tail' valu false)
    | Def'(head, valu) -> Def'( head , p_expr_tail' valu t_p )
    | Or'(listOfexprs) -> Or'(seq_tp_helper listOfexprs t_p)
    | If'(test,dit,dif) ->
         If'( p_expr_tail' test false,
              p_expr_tail' dit t_p,
              p_expr_tail' dif t_p )
    | LambdaSimple'(lambdaParams, body) ->
        LambdaSimple'(lambdaParams , p_expr_tail' body true )
    | LambdaOpt'(lambdaParams,vs, body) -> 
        LambdaOpt'(lambdaParams , vs, p_expr_tail' body true)
    | Applic'(rator, rands) -> if t_p
        then ApplicTP'( (p_expr_tail' rator false),
                        (seq_tp_helper rands false) )
        else Applic'  ( (p_expr_tail' rator false),
                        (seq_tp_helper rands false) )
    | any -> e
  
  
  and seq_tp_helper listOfexprs t_p = 
    match listOfexprs with
    | [] -> []
    | a :: [] -> [p_expr_tail' a t_p]
    | a :: b -> List.append [p_expr_tail' a false] (seq_tp_helper b t_p)
  
  
  ;;
  
  
  (*
  p_expr_tail' 
  (Seq' [Var' (VarFree("+")); Const' (Sexpr (Number (Int 1))); Const' (Sexpr (Number (Int 2)))]) (true);;
  *)
  
  
  
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  (*       HERE WE ARE FINISHED WITH THE ANNOTATE TAIL CALL!!!    *)
  
  
  
  
  (* AMIT LOOK OVER HERE *)
  
  (* in this part we are implementing section 3.4 of the assignment  *)
  (* for each expr' e we check if var should be replaced with BoxGet,*)
  (* and if set shoulb be replaced with BoxSet.                      *)
  (* additionally, for every lambda, compute relevant boxed_symbols  *)
  (* and add a setting line for var to be boxVar                     *)
  (* boxed_symbols are strings of vars that should be boxed.         *)
  (* edited in each lambda that get parameters who should be boxed.  *)
  
  let counter = ref 0;;
  let counterToZero() = counter := 0 ;;
  let counterInc() = counter := !counter + 1 ;;
  let counterDec() = counter := !counter - 1 ;;
  let getCounter() = counter.contents;;
  
  let rec p_expr_box' e boxed_symbols =
    match e with 
    | Const'(sexpr) -> e
    | Seq'(listOfexprs) -> Seq'(box_seq_helper listOfexprs boxed_symbols)
    | Def'(head, valu) -> Def'( head , (p_expr_box' valu boxed_symbols) )
    | Or'(listOfexprs) -> Or'(box_seq_helper listOfexprs boxed_symbols )
    | Box'(name) -> e
    | BoxGet'(name) -> e
    | BoxSet'(name,valu) -> e
    | If'(test,dit,dif) ->
         If'( p_expr_box' test boxed_symbols,
              p_expr_box' dit boxed_symbols,
              p_expr_box' dif boxed_symbols )
    | Applic'(rator, rands) -> 
        Applic'( (p_expr_box' rator boxed_symbols),
                 (box_seq_helper rands boxed_symbols)  )
    | ApplicTP'(rator, rands) -> 
        ApplicTP'( (p_expr_box' rator boxed_symbols),
                   (box_seq_helper rands boxed_symbols) ) 
    
    | Var'(name) -> (box_var_helper e boxed_symbols)
    | Set'(vari, valu) -> (box_set_helper e boxed_symbols)
    
    | LambdaSimple'(lambdaParams, body) -> 
        (box_lambda_helper e boxed_symbols)
    | LambdaOpt'(lambdaParams,vs, body) -> 
        (box_lambda_helper e boxed_symbols)
    
  
  (* seq should be wraped with boxed_symbols in each mapping *)
  and box_seq_helper listOfexprs boxed_symbols = 
    let box_mapper elem = (p_expr_box' elem boxed_symbols) in
    (List.map box_mapper listOfexprs)
  
  
  (* var should be checked if inside boxed_symbols in order to determind *)
  (* whether it should be replaced with BoxGet *)
  and box_var_helper var_case boxed_symbols =
    match var_case with
    | Var'(VarFree(name)) -> Var'(VarFree(name))
    | Var'(var_bound_param) -> 
        (change_var_helper var_bound_param "box_get" boxed_symbols (Const' Void))
    | any -> raise X_syntax_error
  
  (* NOTES:
  -what problem is this solving?
  -why not box varfree?
  
  *)
  
  
  
  (* set should be checked if inside boxed_symbols in order to determind *)
  (* whether it should be replaced with BoxSet *)
  (* body of set should be investigated seperately *)
  and box_set_helper set_case boxed_symbols =
    match set_case with
    | Set'(Var'(VarFree(name)), body ) ->
        Set'(Var'(VarFree(name)), p_expr_box' body boxed_symbols )
    | Set'(Var'(head),Box'(tail)) ->
        Set'(Var'(head),Box'(tail))
    | Set'(Var'(var_bound_param), body ) -> 
        (change_var_helper var_bound_param "box_set" boxed_symbols body)
    | any -> raise X_syntax_error
  
  
  (* if name is inside boxed_symbols, it should be replaced with Box *)
  (* typ is indicator for get and set *)
  and change_var_helper var_bound_param typ boxed_symbols body =
    let extract_name vbp = ( 
      match vbp with
      | VarParam(name,min) -> name
      | VarBound(name,maj,min) -> name
      | any -> raise X_syntax_error     ) in
  
    let var_name = extract_name var_bound_param in
    let ans = List.mem var_name boxed_symbols in
    match ans,typ with
    | true  , "box_get" -> 
        BoxGet'(var_bound_param)
    | true  , "box_set" -> 
        BoxSet'(var_bound_param, (p_expr_box' body boxed_symbols))
    | false , "box_get" -> 
        Var'(var_bound_param)
    | false , "box_set" -> 
        Set'(Var'(var_bound_param), (p_expr_box' body boxed_symbols))
    | any -> raise X_syntax_error
  
  
  
  
  
  
    
  
  (* each lambda create the same lambda,               *)
  (* body and parameters of lambda needs to be checked *)
  and box_lambda_helper e boxed_symbols =
    match e with
    | LambdaSimple'(lambdaParams, body) ->(
        let boxed_sbody = 
          (box_lambda_simple_helper lambdaParams body boxed_symbols) in
        LambdaSimple'(lambdaParams, boxed_sbody )
      )
      
    | LambdaOpt'(lambdaParams,vs, body) -> (
      let listedParams = (List.append lambdaParams [vs]) in
      let boxed_obody = 
        (box_lambda_simple_helper listedParams body boxed_symbols) in
      LambdaOpt'(lambdaParams,vs, boxed_obody )
      )
    | any -> raise X_syntax_error
  
  
  (* body and parameters of lambda needs to be checked if *)
  (* there is parametes, if the body is Seq or not, and   *)
  (* last, we should send each parameter in lambdaParams  *)
  (* to box_params_of_lambda_helper to see if this param  *)
  (* should be boxed inside the body of the lambda        *)
  (* This Function Is Not Finished Yet                    *)
  and box_lambda_simple_helper lambdaParams body boxed_symbols =
    let filter_param symbol = 
      not(List.mem symbol lambdaParams) in
    let filter_boxed_symbols = 
      (List.filter filter_param boxed_symbols) in
    
    match body with
    | Seq'(listOfexprs) ->(
        let (setBoxParams,new_body) = 
              (param_boxing_body lambdaParams listOfexprs) in
        let new_boxed_symbols =
              (List.append filter_boxed_symbols setBoxParams) in
  
        Seq'( box_seq_helper new_body new_boxed_symbols)
    )
    | body_expr -> (
        let (setBoxParams,new_body) = 
              (param_boxing_body lambdaParams [body_expr]) in
        let new_boxed_symbols =
              (List.append filter_boxed_symbols setBoxParams) in
  
        match setBoxParams with
        | [] -> (p_expr_box' body_expr new_boxed_symbols)
        | a :: b -> (p_expr_box' 
            (Seq'(List.append (setBoxSet setBoxParams 0 []) [body_expr] ))
             new_boxed_symbols )
        
      )
    
  
  
  
  and param_boxing_body lambdaParams listOfexprs =
   (param_boxing_body_helper lambdaParams listOfexprs [] )
  
  
  and param_boxing_body_helper lambdaParams listOfexprs setBoxParams =
    match lambdaParams with
    | [] -> (
      if setBoxParams = [] then (setBoxParams,listOfexprs)
      else (setBoxParams ,
            (List.append (setBoxSet setBoxParams 0 [])
                         [Seq'(listOfexprs)]  ) )
      )
    | a :: b -> (
        let boxedParams = 
          (box_params_of_lambda_helper a listOfexprs) in
        (param_boxing_body_helper b listOfexprs 
                  (List.append setBoxParams boxedParams) )
  
      )
    
  
  
  and setBoxSet setBoxParams varMinor setBoxBody =
    match setBoxParams with
    | [] -> setBoxBody
    | a :: b -> (
          setBoxSet b (varMinor+1) (List.append setBoxBody
      [Set'(Var'(VarParam(a,varMinor)),
            Box'(VarParam(a,varMinor)))])
    )
  
  
   
  
      
  (* the function box_params_of_lambda_helper should be edited *)
  (* to get as input each time a different parameter and check *)
  (* if it should be boxed, if yes, add to boxed_symbols and continue *)
  (* AMIT LOOK UNTILL HERE *)
  
  
  (*this function gets as input parameters and body of lambda *)
  (*this function send as output parameters of lambda
                           to see if each one should be boxed *) 
  
  and box_params_of_lambda_helper lambdaParam body =
    (* let ans = true in *)
    let ans = param_should_be_boxed_pred body lambdaParam in
    match ans with
    | true  -> [lambdaParam]
    | false -> []
   
  
  
    
  
  (*this function gets as input parameter name x and body of lambda *)
  (*this function send as output if x should be box *)
  
  and param_should_be_boxed_pred body x =
    counterToZero();
    let (readList, writeList) = (readWriteScanner body x [] [] false) in 
    match readList,writeList with
    | [], _ -> false
    | _, [] -> false
    | any -> (let ans = (readVsWriteComparator readList writeList) in
          if ans then true
          else (lastChance body x) )
  
  (* This functions counts the instances of get and set of specific parameter 'x' *)
  
  and readWriteScanner body x readList writeList flag= 
    match body with
    | [] -> (readList,writeList)
    | a :: b -> (
          let (rList,wList) = 
            readWriteScanner_helper a x readList writeList flag in
          (readWriteScanner b x rList wList flag )
    )
  
  and readWriteScanner_helper body x readList writeList flag = 
    match body with
    | Const'(sexpr) -> (readList,writeList)
    | Var'(innerVar) -> 
      (var_set_scan_helper body x readList writeList flag) 
    | Seq'(listOfexprs) -> (seq_scan_helper listOfexprs x readList writeList flag)
    | Set'(Var'(vari), valu) -> 
      (var_set_scan_helper body x readList writeList flag) 
    | Def'(head, valu) -> (def_scan_helper head valu x readList writeList flag) 
    | Or'(listOfexprs) -> (seq_scan_helper listOfexprs x readList writeList flag)
    | Box'(name) -> (readList,writeList)
    | BoxGet'(name) -> (readList,writeList)
    | BoxSet'(name,valu) -> (readList,writeList)
    | If'(test,dit,dif) -> (seq_scan_helper [test;dit;dif] x readList writeList flag)
  
    | LambdaSimple'(lambdaParams, bodyOfLambda) ->
      if (List.mem x lambdaParams) then (readList,writeList)
      else 
        ( counterInc() ;
          let rest = 
            (readWriteScanner_helper bodyOfLambda x readList writeList false) in
        ( if (flag) then rest else ( counterDec() ; rest ) )
        )
  
    | LambdaOpt'(lambdaParams,vs, bodyOfLambda) -> 
      if ( (List.mem x lambdaParams) || (x=vs) ) then (readList,writeList)
      else
        ( counterInc() ;
          let rest =
            (readWriteScanner_helper bodyOfLambda x readList writeList false) in
          ( if (flag) then rest else ( counterDec() ; rest ) )
        )
      
    | Applic'(rator, rands) -> 
        (seq_scan_helper (List.append [rator] rands) x readList writeList flag)
  
    | ApplicTP'(rator, rands) -> 
        (seq_scan_helper (List.append [rator] rands) x readList writeList flag)
    | any -> raise X_syntax_error
    
  
  and var_scan_helper head x readList writeList flag =
    let newReadList = (List.append readList [getCounter()]) in
    if ( head = x )
    then (newReadList , writeList)
    else (readList , writeList)
  
  
  and set_scan_helper body head x readList writeList flag =
    let newWriteList = (List.append writeList [getCounter()]) in
    if ( head = x )
    then (readWriteScanner_helper body x readList newWriteList flag)
    else (readWriteScanner_helper body x readList writeList flag)
  
  
  
  
  and var_set_scan_helper head x readList writeList flag =
    match head with
    | Var'(VarFree(name)) -> (readList , writeList)
    | Var'(VarParam(name,min)) -> 
        (var_scan_helper name x readList writeList flag)
    | Var'(VarBound(name,min,maj)) -> 
        (var_scan_helper name x readList writeList flag)
    | Set'(Var'(VarFree(name)),tail) -> 
        (set_scan_helper tail name x readList writeList flag)
    | Set'(Var'(VarParam(name,min)),tail) -> 
        (set_scan_helper tail name x readList writeList flag)
    | Set'(Var'(VarBound(name,min,maj)),tail) -> 
        (set_scan_helper tail name x readList writeList flag)
    | any -> raise X_syntax_error
    
  
  
  and def_scan_helper head valu x readList writeList flag =
  let name = (
    match head with 
  | Var'(VarFree(name)) -> name
  | Var'(VarParam(name, _)) -> name
  | Var'(VarBound(name, z, w)) -> name
  | any -> raise X_syntax_error
  ) in
  if (name = x) then (readList,writeList) 
  else (readWriteScanner_helper valu x readList writeList flag)
  
  (* This function counts the instances of get,set of parameter x in list of exprs *)
  and seq_scan_helper exprList x readList writeList flag =
    match exprList with
    | [] -> (readList,writeList)
    | a :: b -> (
          let (readtmp,writetmp) = 
            (readWriteScanner_helper a x readList writeList flag) in
          (seq_scan_helper b x readtmp writetmp flag)
    )
    
  
  (* this function compares on which levels the gets,sets of parameter x was made *)
  and readVsWriteComparator readList writeList = (*IMPLEMENT MUTUAL DISJUNCTION *)
    let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
    let filteredL1 = diff readList writeList in
    let filteredL2 = diff writeList readList in
    let symmetric_diff = List.append filteredL1 filteredL2 in
    match symmetric_diff with
    | [] -> false
    | any -> true
  
  and lastChance body x =
    counterToZero();
    let (readList, writeList) = (readWriteScanner body x [] [] true) in 
    match readList,writeList with
    | [], _ -> false
    | _, [] -> false
    | any -> (readVsWriteComparator readList writeList )
  
  
  ;;
  
  
  let annotate_lexical_addresses e =
    (p_expr' e [] []) ;; 
  
  let annotate_tail_calls e = 
    (p_expr_tail' e false);;
  
  let box_set e = 
    (p_expr_box' e []);;
  
  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr));;
   
  
  end;;
  
  open PC;;
  open Reader;;
  open Tag_Parser;;
  open Semantics;;
  




  (*
  run_semantics
  (
    tag_parse_expression
    (
      read_sexpr
      
        "(define test (lambda (x) ((lambda (y) (cons (lambda () x ) 
                                              (cons (set! x y) '() ) ) ) 1)))"
      
    )
  )
;;
run_semantics
  (
    Def (Var "test",
 LambdaSimple (["x"],
  Applic
    (LambdaSimple (["y"],
      Applic (Var "cons",
        [LambdaSimple ([],  Var "x");
         Applic (Var "cons",
          [Set (Var "x", Var "y"); Const (Sexpr Nil)])])),
     [Const (Sexpr (Number (Int 1)))])))
  )
;;




Def (Var "test",
 LambdaSimple (["x"],
  Applic
    (LambdaSimple (["y"],
      Applic (Var "cons",
        [LambdaSimple ([],  Var "x");
         Applic (Var "cons",
          [Set (Var "x", Var "y"); Const (Sexpr Nil)])])),
     [Const (Sexpr (Number (Int 1)))])))



  *)

  