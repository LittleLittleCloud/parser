type 'a parser = Parser of (string ->  (string * 'a) option)
                                             
let hd s = String.get s 0
let tail s = String.sub s 1 ((String.length s) - 1)                      

let parse p s = match p with
                  Parser f -> f s

let eps = Parser (fun s -> Some (s, ""))
                                
let item x = Parser (fun s ->  match s with
                               | "" -> None
                               | s  -> if x == hd s
                                       then Some (tail s, x)
                                       else None)

let alt p1 p2 = Parser (fun s -> match (parse p1 s) with
                                   | None -> parse p2 s
                                   | Some r -> Some r)

let seq p1 p2 = Parser (fun s -> match (parse p1 s) with
                                 | None -> None
                                 | Some (s2,x) -> parse (p2 x) s2)

let conc p1 p2 = seq p1 (fun _ -> p2)


let matcher p s = match (parse p s) with
                   | None -> false
                   | Some (r,_) -> match r with
                                   | "" -> true
                                   | _  -> false
                                   (* == not defined for strings *)
                     

let ex0 = item 'c'                     
let ex1 = alt (item 'a') (item 'b')
let ex2 = conc ex0 ex1 

              
                      
let rec exAs _ = alt (seq (item 'a') (fun _ -> exAs())) eps