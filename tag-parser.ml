#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

(* let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;; *)
	
                       
exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)

let p_tagSexpr head tail = 
  match tail with
    | Pair(Symbol ("quote") , Pair(inquote,Nil) ) ->
      Const(Sexpr(TaggedSexpr(head,inquote)))
    | _ -> Const(Sexpr(TaggedSexpr(head,tail)))
    ;; 



let rec p_listSexprToPair head =
  match head with
  | [] -> Nil
  | a :: b -> Pair(a, p_listSexprToPair b )
  ;;



let p_validVar head =
  if not(List.mem head reserved_word_list)
  then Var(head)
  else raise X_syntax_error
  

let rec p_expr sexpr =
  match sexpr with
  | Bool        (head)      -> Const(Sexpr(Bool  (head)))
  | Number      (head)      -> Const(Sexpr(Number(head)))
  | Char        (head)      -> Const(Sexpr(Char  (head)))
  | String      (head)      -> Const(Sexpr(String(head)))
  | Nil                     -> Const(Void)
  | TagRef      (head)      -> Const(Sexpr(TagRef(head)))
  | TaggedSexpr (head,tail) -> ( p_tagSexpr head tail )
  | Pair        (head,tail) -> ( p_pairExpr head tail )
  | Symbol      (head)      -> ( p_validVar head      ) 


and p_pairExpr head tail =
  match head,tail with
  | Symbol (str), _ -> (p_pairSymbol str tail)
  | Pair(a, Nil), Nil -> (p_expr a)
  | Pair(a, Nil), _ -> Applic( p_expr head , p_pairExprToList tail )
  (* | Pair(a,b), Nil ->  p_expr head *)
  | Pair(a,b), Nil ->  Applic(p_expr head, [])
  | Pair(a,b), _ ->  Applic(p_expr head,p_pairExprToList tail)
  | any -> Applic(p_expr head,p_pairExprToList tail)
  (* Seq(p_pairExprToList (Pair(head,tail))) *)



and p_pairExprToList head =
  match head with
  | Nil           -> []
  | Pair(a, Nil)  -> [p_expr a]
  | Pair(a,b)     -> List.append [p_expr a] (p_pairExprToList b)
  | any -> raise X_syntax_error 



and p_pairSymbol head tail =
  match head with
  | "and" -> tag_and tail
  | "begin" -> tag_begin tail
  | "cond" -> tag_cond tail
  | "define" -> tag_define tail
  | "else" -> raise X_syntax_error (* tag_else head *)
  | "if" ->  tag_if tail 
  | "lambda" -> tag_lambda tail
  | "let" -> tag_let tail
  | "let*" -> tag_letStar tail
  | "letrec" -> tag_letrec tail
  | "or" -> tag_or tail
  | "quasiquote" -> tag_quasiquote tail
  | "quote" -> tag_quote tail
  | "set!" -> tag_setBang tail
  | "unquote" -> raise X_syntax_error  (* tag_unquote head *)
  | "unquote-splicing" -> raise X_syntax_error (* tag_unquoteSplicing head *)
  | any -> (tag_applic head tail)



and tag_begin head = 
  match head with
  | Nil -> Const(Void)
  | Pair(a, Nil) -> (p_expr a)
  | Pair(a,b) -> Seq(p_pairExprToList head)
  | any -> raise X_syntax_error


and tag_applic head tail =
  Applic( Var(head) , (p_pairExprToList tail) )
  

and tag_if head =
  match head with
  | Pair( test , Pair( dit , Nil) ) ->
    If( p_expr test , p_expr dit , Const(Void) )
  | Pair( test , Pair( dit , Pair( dif , Nil)) ) ->
    If( p_expr test , p_expr dit , p_expr dif )
  | any -> raise X_syntax_error


                    
and tag_or head =
  match head with
  | Nil -> Const(Sexpr(Bool(false)))
  | Pair(a,Nil) -> p_expr a
  | Pair(a,b) ->
    let orList = p_pairExprToList head in
    Or( orList )
  | any -> raise X_syntax_error

and tag_and head =
  match head with
  | Nil -> Const(Sexpr(Bool(true)))
  | Pair(a,Nil) -> p_expr a
  | Pair(a,b) ->
    If( p_expr a, tag_and b, Const(Sexpr(Bool(false))) )
  | any -> raise X_syntax_error


and tag_lambda exp =
  match exp with
  | Pair(head,tail) -> lambdaCreator head tail
  | any -> raise X_syntax_error 

and lambdaCreator head tail =
  (* let (head,tail) = exp in  (* check if head = properlist or improper or empty, and return it as stringlist *) *)
  let (args, listType, str) = (argsListCreator head "" [] ) in
  match listType with
  | "proper" -> 
    (
      match tail with
      | Pair(one,Nil) -> LambdaSimple(args, (p_expr one))
      | Pair(one,many) -> LambdaSimple(args, Seq(p_pairExprToList tail))
      | any -> raise X_syntax_error 
       
    )
  | "improper" -> 
    (
      match tail with
      | Pair(one,Nil) -> LambdaOpt(args, str, (p_expr one))
      | Pair(one,many) -> LambdaOpt(args, str, Seq(p_pairExprToList tail))
      | any -> raise X_syntax_error 
    )
  | any -> raise X_syntax_error

and argsListCreator arglist listType strList = 
  match arglist with 
  | Nil -> (strList, "proper", "")
  | Symbol(y) -> 
    (     if not( List.mem y strList)
          then (strList, "improper", y) 
          else raise X_syntax_error 
    )
  | Pair(Symbol(x) ,y) -> 
    (     if not( List.mem x strList)
          then (argsListCreator y "" (List.append strList [x])) 
          else raise X_syntax_error 
    )
  | any -> raise X_syntax_error

  

and tag_quote head =
  match head with
  | Pair(Nil,Nil) -> Const(Sexpr(Nil))
  | Pair(a,Nil) -> Const(Sexpr(a))
  | any -> raise X_syntax_error





and tag_quasiquote head = 
  match head with
  | Pair(a, Nil) -> p_expr (tag_quasiquoteHelper a)
  | any -> raise X_syntax_error  
  
and tag_quasiquoteHelper head = 
  match head with
  | Nil ->                                                          Pair(Symbol("quote"),Pair(Nil,Nil))
  | Symbol(a) ->                                                    Pair(Symbol("quote"),Pair(Symbol(a),Nil))
  | Pair(Symbol("unquote"), Pair(a,Nil)) ->                         a
  | Pair(Symbol("unquote-splicing"),Pair(a,Nil)) ->                 raise X_syntax_error
  | Pair(a,b) ->                                                    
    (match a with
    | Pair(Symbol("unquote-splicing"), Pair(aa,Nil)) ->             Pair(Symbol("append"), Pair(aa, Pair((tag_quasiquoteHelper b), Nil)))
    | any ->
      match b with
      | Pair(Symbol("unquote-splicing"), Pair(bb,Nil)) ->           Pair(Symbol("cons"), Pair((tag_quasiquoteHelper a), Pair(bb, Nil)))
      | any ->                                                      Pair(Symbol("cons"), Pair((tag_quasiquoteHelper a), Pair((tag_quasiquoteHelper b), Nil)))
    )
  | any ->                                                          head


and tag_define head =
  match head with
  | Nil -> raise X_syntax_error
  | Pair( Pair(a,b) , Pair( c  ,Nil ) ) -> tag_MITdefineHelper a b (Pair(c,Nil))
  | Pair( a , Pair( b  ,Nil ) ) -> tag_defineHelper a b
  | Pair( a , Nil ) -> tag_defineHelper a Nil
  | Pair( Pair(a,b) ,Pair(c,d) ) -> tag_MITdefineHelper a b (Pair(c,d))
  | a -> raise X_syntax_error
  
  

and tag_defineHelper head tail =
  let varName = p_expr head in
  match varName with
  | Var(a) -> Def( varName , p_expr tail )
  (* | Symbol(a) -> Def( varName , p_expr tail ) *)
  | _ -> raise X_syntax_error 


and tag_MITdefineHelper head mid tail =
  let varName = p_expr head in
  let innerLambda = Pair( mid , tail ) in
  let lambdaForm = Pair( Symbol "lambda", innerLambda  )  in
  match varName with
  | Var(a) -> Def( varName , p_expr lambdaForm )
  | _ -> raise X_syntax_error 



and tag_setBang head =
  match head with
  | Nil -> raise X_syntax_error
  | Pair( a , Pair(b,Nil) ) -> tag_setHelper a b
  | Pair( a , Nil ) -> raise X_syntax_error
  | a -> raise X_syntax_error


and tag_setHelper head tail =
  let varName = p_expr head in
  match varName with
  | Var(a) -> Set( varName , p_expr tail )
  | _ -> raise X_syntax_error 



and tag_let head =
  match head with
  | Pair(Nil    ,body) -> 
      let lambdaForm = Pair( Nil, body ) in
      let letLambdaForm = Pair(Pair( Symbol "lambda" , lambdaForm ),Nil) in
      
      p_expr letLambdaForm 

  | Pair(var_val,body) -> 
      let (varList,valList) = (tag_letHelper var_val [] []) in
      let varListPaired = p_listSexprToPair varList in
      let valListPaired = p_listSexprToPair valList in
      
      let lambdaForm = Pair(varListPaired, body ) in
      let letLambdaForm = Pair( Symbol "lambda" , lambdaForm ) in
      let letLambdaFormVals = Pair( letLambdaForm, valListPaired) in

      p_expr letLambdaFormVals 
  
  | _ -> raise X_syntax_error
  
  


and tag_letHelper head letVars letVals=
  match head with
  | Pair( Pair( a, Pair(b,Nil) ) , Nil ) ->
      ( List.append letVars [a] , List.append letVals [b] )
  
  | Pair( Pair( a, Pair(b,Nil) ) , tail ) ->
    tag_letHelper tail (List.append letVars [a])
                       (List.append letVals [b])
  | _ -> raise X_syntax_error





and tag_letStar head = 
  (p_expr (tag_letStarHelper head )) 


and tag_letStarHelper head =
  match head with
  | Pair(Nil    ,body) -> 
      let letForm = Pair( Nil, body ) in
      let letStarForm = Pair( Symbol "let" , letForm ) in
      
      letStarForm 

  | Pair( Pair(var_val,Nil), body) ->
      let letStarForm = Pair( Symbol "let" , 
          Pair(Pair(var_val,Nil),  body ) ) in
      
      letStarForm  

  | Pair(Pair(var_val,restVar_val),body) -> 
      let bodyForm = Pair( restVar_val, body ) in
      let recursive = tag_letStarHelper bodyForm in
      let letStarForm = Pair( Symbol "let" , 
          Pair( Pair(var_val,Nil) , Pair(recursive,Nil) ) ) in
      
      
      letStarForm  
  
  | any -> raise X_syntax_error



 
and tag_letrec head =
  match head with
  | Pair(Nil    ,Pair(body,Nil)) -> 
      let letForm = Pair( Nil, Pair(body,Nil))  in
      let letRecForm = Pair( Symbol "let" , letForm ) in
      
      p_expr letRecForm 

  | Pair(var_val,Pair(body,Nil)) -> 
  
      let (varList,valList) = (tag_letHelper var_val [] []) in
      
      let whateverVarList = List.map (function var -> (Pair (var,
      Pair (Pair (Symbol "quote", Pair (Symbol "whatever", Nil)), Nil)) )) varList in
      
      let setVarList = (letRecHelper varList valList []) in
      
      let whateverListPaired = p_listSexprToPair whateverVarList in
      
      let setVarListPaired = p_listSexprToPair 
        (List.append setVarList [body] )  in
      
      let letForm = Pair( whateverListPaired, setVarListPaired ) in
      let letRecForm = Pair( Symbol "let" , letForm ) in 

      p_expr letRecForm

  | _ -> raise X_syntax_error
  
      

and letRecHelper head tail setList =
  match head, tail with
  | [] , [] -> setList
  | p_var :: restVar , p_val :: restVal  ->  
      (letRecHelper restVar restVal (List.append setList 
      [Pair (Symbol "set!", Pair(p_var, Pair (p_val, Nil)))]))
  | _ , _ -> raise X_syntax_error


      


and tag_cond head = 
  match head with
  | Pair(Pair( Symbol "else" , body ), tail) ->
    p_expr (tag_condCaseC body)
  
  | Pair(Pair(cond,Pair( Symbol "=>" , body ) ) , tail ) -> 
    p_expr (tag_condCaseB cond body tail) 
  
  | Pair(Pair(cond,body) , tail ) -> 
    p_expr (tag_condCaseA cond body tail) 
  
  | _ -> raise X_syntax_error
  



and tag_condCaseA cond body tail =
  match tail with
  | Nil -> 
    Pair(Symbol "if", Pair(cond, Pair(Pair(Symbol "begin", body), Nil)))
  | nextRib -> 
    Pair(Symbol "if", Pair(cond, Pair(Pair(Symbol "begin", body), 
          Pair(Pair(Symbol "cond" , nextRib ), Nil))))
 
                                 
and tag_condCaseC body =
  Pair(Symbol "begin", body)

  
and tag_condCaseB cond body tail =
  match tail with
  | Nil -> 
    Pair(Symbol "let", 
      Pair(
      Pair( Pair(Symbol "value", Pair(cond,Nil)),
            Pair(Pair(Symbol "f", 
                 Pair(Pair(Symbol "lambda",
                      Pair(Nil, body)),
                     
                Nil )), Nil )),
      Pair(Pair(Symbol "if",
         Pair( Symbol "value", 
              Pair(Pair(Pair(Symbol "f", Nil),
                   Pair(Symbol "value", Nil)), Nil))), Nil)))


  | nextRib -> 
    Pair(Symbol "let", 
      Pair(
      Pair( Pair(Symbol "value", Pair(cond,Nil)),
            Pair(Pair(Symbol "f", 
                 Pair(Pair(Symbol "lambda",
                      Pair(Nil, body)), Nil )),
            Pair(Pair(Symbol "rest", 
                 Pair(Pair(Symbol "lambda",
                      Pair(Nil, Pair(Pair(Symbol "cond" ,nextRib),
                
                     
                Nil))), Nil )), Nil ))),
      Pair(Pair(Symbol "if",
         Pair( Symbol "value", 
              Pair(Pair(Pair(Symbol "f", Nil),
                   Pair(Symbol "value", Nil)), 
                   Pair(Pair(Symbol "rest", Nil), Nil)))), Nil)))
  
  ;;


let tag_parse_expression sexpr = p_expr sexpr;;

let tag_parse_expressions sexpr = List.map tag_parse_expression sexpr;;

end;; (* struct Tag_Parser *)


(*     here are the 4 tests we couldn't pass with same error 








tag_parse_expressions

[Pair (String "should not", Pair (String "be", Pair (String "list", Nil)))]

;;


[Seq
  [Const (Sexpr (String "should not")); Const (Sexpr (String "be"));
   Const (Sexpr (String "list"))]]


[Applic (Const (Sexpr (String "should not")),[Const (Sexpr (String "be"));
         Const (Sexpr (String "list"))])]




tag_parse_expressions
[Pair (Symbol "letrec",
    Pair(Pair(Pair (Symbol "x",
        Pair(Pair (TaggedSexpr ("y",
           Pair (Symbol "quote", Pair (Nil, Nil))), Nil),Nil)),
        Pair(Pair (Symbol "y",Pair(Pair (Symbol "begin",
          Pair (Number (Int 1),Pair (Number (Int 2),
          Pair (Number (Int 3), Nil)))),Nil)),Nil)),
        Pair (Pair (Symbol "set", Pair (Symbol "x",
                   Pair (Symbol "y", Nil))), Nil)))]

;;



[Applic
  (LambdaSimple (["x"; "y"],
    Seq
     [Set (Var "x", Seq [Const (Sexpr (TaggedSexpr ("y", Nil)))]);
      Set (Var "y",
       Seq
        [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)));
         Const (Sexpr (Number (Int 3)))]);
      Applic (Var "set", [Var "x"; Var "y"])]),
  [Const (Sexpr (Symbol "whatever")); Const (Sexpr (Symbol "whatever"))])]


[Applic
  (LambdaSimple (["x"; "y"],
    Seq
     [Set (Var "x", Applic (Const (Sexpr (TaggedSexpr ("y", Nil))), []));
      Set (Var "y",
       Seq
        [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)));
         Const (Sexpr (Number (Int 3)))]);
      Applic (Var "set", [Var "x"; Var "y"])]),
  [Const (Sexpr (Symbol "whatever")); Const (Sexpr (Symbol "whatever"))])]



tag_parse_expressions
[Pair (Symbol "letrec",
      Pair(Pair(Pair (Symbol "x",
            Pair(Pair (TaggedSexpr ("y",
             Pair (Symbol "quote", Pair (Nil, Nil))), Nil),
             Nil)),Nil),Pair (Symbol "x", Nil)))];;


[Applic
  (LambdaSimple (["x"],
    Seq
     [Set (Var "x", 
        Seq [Const (Sexpr (TaggedSexpr ("y", Nil)))]); Var "x"]),
  [Const (Sexpr (Symbol "whatever"))])]



[Applic
  (LambdaSimple (["x"],
    Seq
      [Set (Var "x", 
        Applic (Const (Sexpr (TaggedSexpr ("y", Nil))), [])); Var "x"]),
  [Const (Sexpr (Symbol "whatever"))])]



tag_parse_expressions [TaggedSexpr ("foo",
  Pair (Number (Int 1), Pair (Number (Int 2), Pair (Number (Int 3), Nil))));
  Pair (Number (Int 1),Pair (TaggedSexpr ("foo", Number (Int 2)),
     Pair (TagRef "foo", Nil)))] ;;



     [Const
     (Sexpr
       (TaggedSexpr ("foo",
         Pair (Number (Int 1),
          Pair (Number (Int 2), Pair (Number (Int 3), Nil))))));
    Seq
     [Const (Sexpr (Number (Int 1)));
      Const (Sexpr (TaggedSexpr ("foo", Number (Int 2))));
      Const (Sexpr (TagRef "foo"))]]


     
     
     [Const(Sexpr(
       TaggedSexpr ("foo",
       Pair (Number (Int 1),
       Pair (Number (Int 2), Pair (Number (Int 3), Nil))))));
    
    Applic 
      (Const (Sexpr (Number (Int 1))),
      [Const (Sexpr (TaggedSexpr ("foo", Number (Int 2))));
      Const (Sexpr (TagRef "foo"))])]



      *)