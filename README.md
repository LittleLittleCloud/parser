# parser
transfer the string to an AST tree

example:
input (a+(b-c))
output Plus(Id a,Minus (Id b,Id c))

only supoort the + - and ()
