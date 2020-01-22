
#use "pc.ml";;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Int of int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Int n1), Number(Int n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | TaggedSexpr(name1, expr1), TaggedSexpr(name2, expr2) -> (name1 = name2) && (sexpr_eq expr1 expr2) 
  | TagRef(name1), TagRef(name2) -> name1 = name2
  | _ -> false;;
  
module Reader: sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
  
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

open PC;; 




let p_hash = char '#';; (* This is the # parser *)
let p_fCI = char_ci 'f';;
let p_tCI = char_ci 't';;

let p_plus = char '+' ;; (* This is the + parser *)
let p_minus = char '-' ;; (* This is the - parser *)
let p_dot = char '.' ;;
let p_eCI = PC.char_ci 'e';;
let p_rCI = PC.char_ci 'r';;

let p_digit = range '0' '9' ;;
let p_letters = range 'a' 'z' ;;
let p_Bletters = pack (range 'A' 'Z') (function ch -> lowercase_ascii ch) ;;

let p_backSlash = char '\\' ;;
let p_nul = word_ci "nul";;
let p_newline = word_ci "newline";;
let p_return = word_ci "return";;
let p_tab = word_ci "tab";;
let p_page = word_ci "page";;
let p_space = word_ci "space";;

let p_metaNewline = word_ci "\\n" ;;
let p_metaReturn =  word_ci "\\r" ;;
let p_metaTab =  word_ci "\\t" ;;
let p_metaPage =  word_ci "\\f" ;;
let p_metaBackSlash =  word_ci "\\\\" ;; 
let p_metaDoubleQuote =  word_ci "\\\"" ;;

let p_punctuations = disj_list [ ( char '!' ) ; ( char '$' ) ;
( char '^' ) ; ( char '*' ) ;    ( char '-' ) ; ( char '_' ) ;
( char '=' ) ; ( char '+' ) ;    ( char '<' ) ; ( char '>' ) ;
( char '/' ) ; ( char '?' ) ;    ( char ':' )                 ];;

let p_Rquote = char '\'' ;;
let p_Qquote = char '`' ;;
let p_USquote = word_ci ",@" ;;
let p_Uquote = char ',' ;;
let p_Lparen = char '(' ;;
let p_Rparen = char ')' ;;
let p_LBparen = char '{' ;;
let p_RBparen = char '}' ;;
let p_equal = char '=' ;;





(* boolean exps *)

let p_True = caten p_hash p_tCI ;; (* This is the PC of #t and #T that symbol 'True' value *)
let p_False = caten p_hash p_fCI ;;
let p_bool = (disj p_True p_False);; 

let boolCreator =
  pack p_bool (function(head, tail) ->  
                   match tail with
                   | 't' -> Bool(true)
                   | 'T' -> Bool(true)
                   | 'f' -> Bool(false)
                   | 'F' -> Bool(false)
                   | any -> raise PC.X_no_match);;




(* character expressions *)

let p_charPrefix = caten p_hash p_backSlash ;; (* This is the #\ parser *)
let p_visibleSimpleChar = function
                          | [] -> raise X_no_match
                          | c :: rest ->
                          if int_of_char c > 32 then (c, rest)
                          else raise X_no_match ;;

let p_visibleChar = caten p_charPrefix p_visibleSimpleChar;;

let namedCharsList = [p_nul; p_newline; p_return; p_tab; p_page; p_space] ;;

let p_namedCharCode = pack (disj_list namedCharsList)
                  ( function charList -> 
                    match (list_to_string 
                      (List.map lowercase_ascii charList)) with
                    | "nul" -> (char_of_int 0)
                    | "newline" -> (char_of_int 10)
                    | "return" -> (char_of_int 13)
                    | "tab" -> (char_of_int 9)
                    | "page" -> (char_of_int 12)
                    | "space" -> (char_of_int 32)
                    | any -> raise PC.X_no_match
                    ) ;;


let p_namedChar =  caten p_charPrefix p_namedCharCode ;;

let p_character = disj p_namedChar p_visibleChar ;;
let p_chars = star p_character ;;

let charCreator =
  pack p_character  (function(head, tail) ->  (*   head will be the tuple ( (#,\) ,CHAR )  *)
              Char( tail )
  );; 


(* meta chars - in strings *)



let metaCharsList = [p_metaNewline; p_metaReturn; p_metaTab; p_metaPage; p_metaBackSlash; p_metaDoubleQuote] ;;
let p_metaChar = pack (disj_list metaCharsList)
                    ( function charList -> 
                        match (list_to_string 
                              (List.map lowercase_ascii charList)) with
                        | "\\n" -> (char_of_int 10)
                        | "\\r" -> (char_of_int 13)
                        | "\\t" -> (char_of_int 9)
                        | "\\f" -> (char_of_int 12)
                        | "\\\\" -> (char_of_int 92)
                        | "\\\"" -> (char_of_int 34)
                        | any -> raise PC.X_no_match
                    ) ;;


let p_stringLiteral = disj_list[ (range (char_of_int 0 ) (char_of_int 33) );
                                 (range (char_of_int 35) (char_of_int 91) );
                                 (range (char_of_int 93) (char_of_int 127)) ];;

let p_stringChar = star (disj p_metaChar p_stringLiteral);;

let p_stringSentence = caten (char (char_of_int 34)) 
                             (caten p_stringChar (char (char_of_int 34)) ) ;;

let stringCreator =
pack p_stringSentence  (function(head,(word,tail)) ->
            String( list_to_string word )
);; 



(*                *)
(* Symbols *)



let p_symbolChar = disj_list[ p_punctuations ; p_digit ;
p_letters; p_Bletters    ] ;;
                              

let symbolCreator = pack (plus p_symbolChar) (function sym ->
    Symbol( list_to_string sym ) ) ;;



(* symbolCreator (string_to_list "AASdaa213123ba assadsaf" );;  *)
(* Symbols *)
(* *)

let p_NumSign = disj p_plus p_minus ;; 
let dotCreator = pack p_dot ( function dot -> ['.']) ;;
let eCreator = pack p_eCI ( function e -> e ) ;;
let rCreator = pack p_rCI ( function e -> e ) ;;


let p_natural = plus p_digit ;;
let p_integer = caten ( maybe p_NumSign ) p_natural ;;
let p_float = caten_list 
    [ p_natural ; dotCreator; p_natural] ;;


let integerNum = pack p_integer
(function ( head , tail) -> 
    match head with
    | Some '-' -> -1 * int_of_string( list_to_string tail )
    | any ->   int_of_string( list_to_string tail )
  ) ;;


let floatNum = pack (caten ( maybe p_NumSign ) p_float )
(function ( head, tail ) -> 
    match head with
    | Some '-' -> -1.0 *. 
    float_of_string( list_to_string (List.concat tail) )
    | any ->   
    float_of_string( list_to_string (List.concat tail) )
  ) ;;


let intCreator = 
  pack integerNum ( function num -> Number(Int(num)));;


let floatCreator = 
  pack floatNum ( function num -> Number(Float(num)));;


let scientINum = pack 
  ( caten ( caten integerNum eCreator) integerNum )
  ( function ( (inta, e ), intb ) ->
      let f=((float_of_int inta) *. (10. ** (float_of_int intb))) in
        Number( Float( f ) )
  );;


let scientFNum = pack 
  ( caten ( caten floatNum eCreator) integerNum )
  ( function ( (inta, e ), intb ) ->
      let f=(inta *. (10. ** (float_of_int intb))) in
        Number( Float( f ) )
  );;



let naturalRadixNum = pack p_natural 
(function head -> let rad = int_of_string( list_to_string head ) in
                  if rad > 36 then raise X_no_match else rad ) ;;


let p_Rdigit = disj_list[ p_letters; p_Bletters; p_digit ];;
let p_radix  = plus p_Rdigit ;;

let p_Iradix = caten ( maybe p_NumSign ) p_radix ;;
let p_Fradix = caten p_Iradix ( caten dotCreator p_radix) ;;

let p_structIradix = (caten p_hash 
    ( caten ( caten naturalRadixNum rCreator) p_Iradix ) );;
let p_structFradix = (caten p_hash 
    ( caten ( caten naturalRadixNum rCreator) ( p_Fradix ) ) );;


let radixINum = pack (p_structIradix)
  (function ( hash, ( (rad, r ), (sign,num) ) ) ->
          let numCalc = List.map 
          (function ch -> int_of_char ch ) num in
          let numVal = List.map
          (function x -> if x > 96 then x-87 else x-48 ) numCalc in
          let numValVerify = List.map
          (function x -> if x >= rad then raise X_no_match else x ) numVal in
          let base a b = a*rad + b in
          let numRadVal = List.fold_left base 0 numValVerify in


      match sign with
        | Some '-' -> Number( Int(-1 * numRadVal ) )
            
        | any ->   Number( Int(numRadVal ) )
      
  );;


let radixFNum = pack (p_structFradix)
  (function ( hash, ( (rad, r ), ((sign,num),(dot,frac) ) ) ) ->
          let base a b = a*rad + b in
          let baseF a b = ( (float_of_int a) +. b )/. (float_of_int rad) in
          let baseToNum x = if x > 96 then x-87 else x-48 in 
          let baseVerify x = if x >= rad then raise X_no_match else x in
          
          let numCalc       = List.map int_of_char num in
          let numVal        = List.map baseToNum numCalc in
          let numValVerify  = List.map baseVerify numVal in
          let numRadVal     = List.fold_left base 0 numValVerify in

          let fracCalc      = List.map int_of_char frac in
          let fracVal       = List.map baseToNum fracCalc in
          let fracValVerify = List.map baseVerify fracVal in
          let fracRadVal    = List.fold_right baseF fracValVerify 0.0 in
          
          
      match sign with
        | Some '-' -> Number( Float(
          -1.0 *. ( (float_of_int numRadVal)  +.  fracRadVal)))    
        | any ->  Number( Float(
                  ( (float_of_int numRadVal)  +.  fracRadVal)))
      
  );;  



let numbersCreator = disj_list[ radixFNum ; radixINum ; 
          scientFNum ; scientINum ; floatCreator ; intCreator] ;;


(*                *)
(*                *)
(* Sexpr *)

(* skippables *)



(* whitespaces and comments *)
(* whitespace & semicolon *)
let pred_whiteSpace = fun ch -> int_of_char ch < 33 ;; (* all chars that we can skip *)
let p_whiteSpace = pack (const pred_whiteSpace) (function input-> ()) ;; 
let pred_semicolon = fun ch -> int_of_char ch = 59 ;;
let p_semicolon = const pred_semicolon ;;

(* end of input/end of line *)
let pred_endOfLine = fun ch -> int_of_char ch = 10 ;;
let p_endOfLine = const pred_endOfLine ;;
let packer_endOfLine = pack p_endOfLine (function newline -> [(char_of_int 10)]);;
let p_endOfComment = disj nt_end_of_input packer_endOfLine ;;



(* comment *)
let p_lineComment = 
  let nt_endComment = disj packer_endOfLine nt_end_of_input in
  let nt_char = diff nt_any nt_endComment in
  let nt_charStar = star nt_char in 
  let nt_comment = caten p_semicolon nt_charStar in 
  let nt_comment = caten nt_comment nt_endComment in
  let nt = pack nt_comment (function input-> ()) in 
  nt ;;
  


(* whitespaces and comments *)
(* whitespace & semicolon *)

let rec p_sexpr str = 
  let cfg =  pack (disj_list[ 
    (not_followed_by (caten p_skip boolCreator)     p_symbolChar);
    (caten p_skip charCreator);
    (not_followed_by (caten p_skip numbersCreator)  p_symbolChar); 
    (not_followed_by (caten p_skip stringCreator)   p_symbolChar);                                                                            
    (not_followed_by (caten p_skip symbolCreator )  p_symbolChar);
    (not_followed_by (caten p_skip quoteRForm)      p_symbolChar);
    (not_followed_by (caten p_skip quoteQForm)      p_symbolChar);
    (not_followed_by (caten p_skip quoteUSForm)     p_symbolChar);
    (not_followed_by (caten p_skip quoteUForm)      p_symbolChar);
    (* (not_followed_by (caten p_skip listForm)        p_symbolChar);
    (not_followed_by (caten p_skip listDotForm)     p_symbolChar); *)
    (caten p_skip listForm)        ;
    (caten p_skip listDotForm)     ;
    (not_followed_by (caten p_skip tagExprForm)     p_symbolChar)
    
    ])
          (function (head,tail) -> tail )  in

      cfg str

and p_sexpComment str = 
  let p_sexpCommentStart = caten p_hash p_semicolon in
  let p_singleSexpComment = caten p_sexpCommentStart p_sexpr in 
  pack p_singleSexpComment (function ((hash,semicolon), singleSexp)->()
  ) str

and p_skip str = 
  let skip = disj_list [ p_sexpComment ; p_whiteSpace ; p_lineComment ] in
  let skips = star skip in
  skips str


and nilCreator str =
  let p_nil = caten p_Lparen (caten p_skip p_Rparen) in
  pack p_nil (function input -> Nil) str



and tagExprCheck s n =
  match s with
  | TaggedSexpr(name1, expr1) -> 
      if name1 = n then raise X_this_should_not_happen else
      TaggedSexpr(name1, tagExprCheck expr1 n)
  | Pair(car1, cdr1) -> 
      Pair( tagExprCheck car1 n , tagExprCheck cdr1 n )
  | _ -> s



and tagExprForm str=
  let p_tag = caten p_hash (caten p_LBparen 
            ( caten (plus p_symbolChar) p_RBparen) ) in
  let p_eqExpression = caten p_equal p_sexpr in
  let p_tagExpr = caten p_tag (maybe p_eqExpression) in
  pack p_tagExpr 
    (function ((hash ,(lbparen, (symbol , rparen ) )),tail)  ->
    match tail with
    | Some (eq , sone) ->
        let soneChecked = tagExprCheck sone (list_to_string symbol) in
        TaggedSexpr( list_to_string symbol, soneChecked)
    | any -> TagRef( list_to_string symbol)

    ) str




and listForm str=
  let p_list = caten p_Lparen (caten (star p_sexpr) (caten p_skip p_Rparen)) in 
  let p_pairPack sa sb = Pair( sa , sb ) in
  pack p_list 
    (function (lparen,(slist , (ip, rparen) ) )  ->
      List.fold_right p_pairPack slist Nil 
    ) str

    


and listDotForm str=
  let p_listDot = caten p_Lparen ( caten (plus p_sexpr) 
      ( caten (caten p_skip dotCreator) (caten p_sexpr (caten p_skip p_Rparen) ) ) ) in
  let p_pairPack sa sb = Pair( sa , sb ) in
  pack p_listDot 
    (function (lparen, (slist , ( dot, (sone ,rparen) ) ) ) ->
      List.fold_right p_pairPack slist sone 
    ) str
  
  

and quoteRForm str=
  pack (caten p_Rquote p_sexpr)
    (function (q,sexpr) -> 
        Pair( Symbol( "quote") , Pair( sexpr , Nil ) ) 
    ) str
  
and quoteQForm str=
  pack (caten p_Qquote p_sexpr)
    (function (q,sexpr) -> 
        Pair( Symbol( "quasiquote") , Pair( sexpr , Nil ) ) 
    ) str

and quoteUSForm str=
  pack (caten p_USquote p_sexpr)
    (function (q,sexpr) -> 
        Pair( Symbol( "unquote-splicing") , Pair( sexpr , Nil ) ) 
    ) str

and quoteUForm str=
  pack (caten p_Uquote p_sexpr)
    (function (q,sexpr) -> 
        Pair( Symbol( "unquote") , Pair( sexpr , Nil ) ) 
    ) str
    
;;         (* this IMPORTANT ENCLOSE LET REC *)


let read_sexpr string = 
  let ( head , tail ) = (p_sexpr) (string_to_list string) in
  match (head,tail) with
  | (Nil,_) -> Nil
  | (head,[]) -> head
  | (head,tail) -> let (a,b) = p_skip tail in
      if b = [] then head else raise X_no_match ;;

let read_sexprs string = 
  let (head,tail) = (star p_sexpr) (string_to_list string) in
  match (head,tail) with
  | (head,[]) -> head
  | (head,tail) -> let (a,b) = p_skip tail in
      if b = [] then head else raise X_no_match ;; 

end;; (* struct Reader *)


open Reader;;
