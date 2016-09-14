type token = PLUS | MINUS | OPEN | CLOSE | ID of char | EOF
(* AST *)                                      
type exp = Plus of exp*exp | Minus of exp*exp | Id of char |Epi
let rec nextToken s =
     if String.length s == 0
     then (EOF, "")
     else   let rest = String.sub s 1 ((String.length s)-1) in
            let ch = String.get s 0 in
            match ch with
                   | ' '  -> nextToken rest
                   | '-'  -> (MINUS, rest)
                   | '+'  -> (PLUS, rest)
                   | '('  -> (OPEN, rest)
                   | ')'  -> (CLOSE, rest)
                   | x    -> (ID x, rest)

let rec printAST ast=match ast with
| Epi -> ""
| Id c ->String.make 1 c
| Plus(l,r)->String.concat "" ["Plus(";printAST l;",";printAST r;")"]
|Minus(l,r)->String.concat "" ["Minus(";printAST l;",";printAST r;")"]
|_->""

let rec toClose s=let a=nextToken s in match a with
| (CLOSE,rest) -> rest
| (_,rest)-> toClose rest


let rec matchParenthesis result i s=

match (nextToken s) with
| (CLOSE,rest) -> let i=i-1 in if i==0 then result else (let result=(result)^")" in matchParenthesis result i rest)
|(OPEN,rest)->let i=i+1 in let result=result^"(" in matchParenthesis result i rest
|(PLUS,rest)->let result=result^"+" in matchParenthesis result i rest
|(MINUS,rest)->let result=result^"-" in matchParenthesis result i rest
|(ID c,rest)->let result=result^(Char.escaped c) in matchParenthesis result i rest
|(EOF,"")->if i==0 then result else ""
|_->""


let rec e s= 
let rec e' lAST s= match (nextToken s) with
| (PLUS,rest) -> Plus (lAST,e rest)
| (MINUS,rest)-> Minus(lAST,e rest)
|(CLOSE,rest) ->lAST
|(EOF,"")->lAST
|_->lAST
 in


match (nextToken s) with
|(ID c,rest)  -> e' (Id c) rest
| (OPEN,rest)->let str=matchParenthesis "" 1 rest in
				let lAst=e str in 
				(* print_endline str;  *)
				(* print_endline (printAST lAst); *)
				print_endline str;
				print_endline rest;
				let a=String.length rest in
				let b=String.length str in
				let restline=String.sub rest (b+1) (a-b-1) in
				print_endline restline;
				e' lAst restline
|_ -> Epi

