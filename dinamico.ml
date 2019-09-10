(* Identificatori *)
type ide = string;;

(* Espressioni che saranno valutate dall'interprete*)
type exp =
  | EInt of int
  | EBool of bool
  | EString of string
  | Den of ide  
  | Let of ide * exp * exp
  | Sum of exp * exp 
  | Sub of exp * exp
  | Prod of exp * exp
  | Div of exp * exp
  | Minus of exp
  | Equal of exp * exp
  | IsZero of exp
  | Not of exp
  | GreaterThan of exp * exp 
  | LesserThan of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | IfThenElse of exp * exp * exp
  | Fun of ide * exp
  | FunCall of exp * exp
  | LetRec of ide * ide * exp * exp
  | Dict of item
  | Select of exp * ide
  | Insert of exp * ide * exp
  | Remove of exp * ide 
  | Clear of exp
  | ApplyOver of exp * exp
and item = Unbound | KeyValue of ide * exp * item;;    

(* Tipi esprimibili che ottengo dalla valutazione delle exp *)
type evT = 
  | Int of int 
  | Bool of bool
  | String of string
  | FunVal of ide * exp
  | RecFunVal of ide * ide * exp
  | DictVal of (ide * evT) list
  | Unbound
and env = (ide * evT) list;;

(* Crea un nuovo ambiente *)
let bind (amb:env) (x:ide) (value:evT) : env = (x, value)::amb;;

(* Cerca l'identificatore nell'ambiente  *)
let rec lookup (amb:env) (x:ide) : evT = match amb with
  | (y, value)::amb2 -> if (x=y) then value else lookup amb2 x
  | [] -> failwith "not found";;

(* Controlla il tipo *)
let typecheck (s:string) (x:evT) : bool = match s with
  | "int" -> (match x with
              | Int(_) -> true
              | _ -> false)
  | "bool" -> (match x with
                | Bool(_) -> true
                | _ -> false)
  | _ -> failwith "Errore di tipo";;



(* Interprete *)
let rec rt_eval (e:exp) (amb:env) : evT = match e with
  | EInt(e1) -> Int(e1)
  | EBool(e1) -> Bool(e1)
  | EString(s) -> String(s)
  | Den(nome) -> lookup amb nome
  | Let(nome, e1, e2) -> let v1 = rt_eval e1 amb in
                            rt_eval e2 (bind amb nome v1)
  | Sum(e1, e2) -> let v1 = rt_eval e1 amb in
                    let v2 = rt_eval e2 amb in
                    (match typecheck "int" v1, typecheck "int" v2, v1, v2 with
                      | true, true, Int(a), Int(b) -> Int(a + b)
                      | _, _, _, _ -> failwith "Errore di tipo")
  | Sub(e1, e2) -> let v1 = rt_eval e1 amb in
                    let v2 = rt_eval e2 amb in
                      (match typecheck "int" v1, typecheck "int" v2, v1, v2 with
                        | true, true, Int(a), Int(b) -> Int(a - b)
                        | _, _, _, _ -> failwith "Errore di tipo")
  | Prod(e1, e2) -> let v1 = rt_eval e1 amb in
                      let v2 = rt_eval e2 amb in
                        (match typecheck "int" v1, typecheck "int" v2, v1, v2 with
                          | true, true, Int(a), Int(b) -> Int(a * b)
                          | _, _, _, _ -> failwith "Errore di tipo")
  | Div(e1, e2) -> let v1 = rt_eval e1 amb in
                      let v2 = rt_eval e2 amb in
                        (match typecheck "int" v1, typecheck "int" v2, v1, v2 with
                          | true, true, Int(a), Int(b) -> Int(a / b)
                          | _, _, _, _ -> failwith "Errore di tipo")
  | Minus(e1) -> let v1 = rt_eval e1 amb in
                  (match typecheck "int" v1, v1 with
                    | true, Int(x) -> Int(-x)
                    | _, _ -> failwith "Errore di tipo")
  | Equal(e1, e2) -> let v1 = rt_eval e1 amb in 
                      let v2 = rt_eval e2 amb in
                        (match typecheck "int" v1, typecheck "int" v2, v1, v2 with
                          | true, true, Int(x), Int(y) -> Bool(x=y)
                          | _, _, _, _ -> failwith "Errore di tipo")
  | IsZero(e1) -> let v1 = rt_eval e1 amb in
                    (match typecheck "int" v1, v1 with
                      | true, Int(x) -> Bool(x=0)
                      | _, _ -> failwith "Errore di tipo")
  | Not(e1) -> let v = rt_eval e1 amb in
                        (match typecheck "bool" v, v with
                          | true, Bool(x) -> Bool(not x)
                          | _, _ -> failwith "Errore di tipo")
  | GreaterThan(e1, e2) -> let v1 = rt_eval e1 amb in
                            let v2 = rt_eval e2 amb in
                              (match typecheck "int" v1, typecheck "int" v2, v1, v2 with
                                | true, true, Int(x), Int(y) -> Bool(x > y)
                                | _, _, _, _ -> failwith "Errore di tipo")
  | LesserThan(e1, e2) -> let v1 = rt_eval e1 amb in
                            let v2 = rt_eval e2 amb in
                              (match typecheck "int" v1, typecheck "int" v2, v1, v2 with
                                | true, true, Int(x), Int(y) -> Bool(x < y)
                                | _, _, _, _ -> failwith "Errore di tipo")
  | And(e1, e2) -> let v1 = rt_eval e1 amb in
                    let v2 = rt_eval e2 amb in
                      (match typecheck "bool" v1, typecheck "bool" v2, v1, v2 with
                        | true, true, Bool(x), Bool(y) -> Bool(x && y)
                        | _, _, _, _ -> failwith "Errore di tipo")
  | Or(e1, e2) -> let v1 = rt_eval e2 amb in
                    let v2 = rt_eval e2 amb in
                      (match typecheck "bool" v1, typecheck "bool" v2, v1, v2 with
                        | true, true, Bool(x), Bool(y) -> Bool(x || y)
                        | _, _, _, _ -> failwith "Errore di tipo")
  | IfThenElse(c, e1, e2) -> let cond = rt_eval c amb in
                               (match typecheck "bool" cond, cond with
                                  | true, Bool(x) -> if x then rt_eval e1 amb else rt_eval e2 amb
                                  | _, _ -> failwith "Guardia non booleana")
  | Fun(x, e1) -> FunVal(x, e1)
  | LetRec(f, par, fbody, flet) -> let amb2 = bind amb f (RecFunVal(f, par, fbody)) in
                                      rt_eval flet amb2
  | FunCall(e1, x) -> let fclosure = rt_eval e1 amb in
                       (match fclosure with
                        | FunVal(y, e2) -> let v1 = rt_eval x amb in
                                            let amb2 = bind amb y v1 in
                                              rt_eval e2 amb2
                        | RecFunVal(f, par, fbody) -> let v = rt_eval x amb in
                                                        let ambric = bind amb f fclosure in
                                                          let amb2 = bind ambric par v in 
                                                            rt_eval fbody amb2
                        | _ -> failwith("Non e' una funzione"))
  | Dict(e1) -> let d = rt_evalDict e1 amb in
                  if(check d) then DictVal(d) else failwith "Chiavi duplicate"
  | Select(dict, i) -> let d = rt_eval dict amb in 
                      (match d with
                        | DictVal(l) -> search l i
                        | _ -> failwith "Non è un dizionario")
  | Insert(dict, i, e1) -> let d = rt_eval dict amb in
                              (match d with
                                | DictVal(l) -> DictVal(add l i (rt_eval e1 amb))
                                | _ -> failwith "Non è un dizionario")
  | Remove(dict, i) -> let d = rt_eval dict amb in
                          (match d with
                            | DictVal(l) -> DictVal(del l i)
                            | _ -> failwith "Non è un dizionario")
  | Clear(dict) -> DictVal([])
  | ApplyOver(f, dict) -> let d = rt_eval dict amb in 
                            (match d with
                              | DictVal(l) -> DictVal(app l f amb)
                              | _ -> failwith "Non è un dizionario")                        
and
  rt_evalDict e amb =
    match e with
      | KeyValue(i, e1, it2) -> let v1 = rt_eval e1 amb in
                                  (i, v1)::rt_evalDict it2 amb
      | Unbound -> []
and
  del d i = 
    match d with
      | (key, value)::l2 -> if (key = i) then l2 else (key, value)::del l2 i
      | [] -> []
and
  add d i v = match d with
    | [] -> [(i, v)]
    (* Se si inserisce un elemento già ripetuto aggiorno il valore dell'elemento presente nel dizionario *)
    | (key, value)::xs -> if key=i then (key, v)::xs else (key, value)::add xs i v  
and
  app d f amb = match d with
    | [] -> []
    | (key, value)::xs -> let fClosure = rt_eval f amb in
                (match fClosure with
                  | FunVal(par, body) -> (key, rt_eval body (bind amb par value))::app xs f amb
                  | RecFunVal(nome, par, body) -> (key, rt_eval body (bind (bind amb nome fClosure) par value))::app xs f amb
                  | _ -> failwith "Non è una funzione")
and
  search d i = match d with
    | [] -> Unbound
    | (key, value)::xs -> if key=i then value else search xs i
and
  check d =
    let rec isin i dict =
      match dict with
        | [] -> false
        | (key, value)::xs -> if i = key then true else isin i xs in
    match d with
      | [] -> true
      | (key, value)::xs -> if(isin key xs) then false else check xs;;    

let emptyenv = [];;

(* Dizionario vuoto *)
rt_eval (Dict(Unbound)) emptyenv;;

(* Dizionario con elementi *)
let dic = Dict(KeyValue("Nome", EString("Alessandro"),
               KeyValue("Cognome", EString("Puccia"),
               KeyValue("Matricola", EInt(547462), 
               KeyValue("Eta", LetRec("fact", "n",
                                      IfThenElse(Equal(Den("n"), EInt(0)), 
                                                 EInt(1),
                                                 Prod(Den("n"), FunCall(Den("fact"), Sub(Den("n"), EInt(1))))), 
                                      FunCall(Den("fact"), EInt(4))),
               KeyValue("Anno Iscrizione", EInt(2016), 
               KeyValue("Residenza", EString("Modica"),
               Unbound)))))));;
rt_eval dic emptyenv;;

(* Dizionario creato con chiavi ripetute *)
let dicR = Dict(KeyValue("Nome", EString("Alessandro"),
               KeyValue("Nome", EString("Giovanni"),
               KeyValue("Matricola", EInt(547462), 
               KeyValue("Eta", Let("y", LetRec("fact", "n",
                                        IfThenElse(Equal(Den("n"), EInt(0)), 
                                                 EInt(1),
                                                 Prod(Den("n"), FunCall(Den("fact"), Sub(Den("n"), EInt(1))))), 
                                        FunCall(Den("fact"), EInt(4))), 
                                        Sub(Den("y"), EInt(3))),
               KeyValue("Anno Iscrizione", EInt(2016), 
               KeyValue("Residenza", EString("Modica"),
               Unbound)))))));;
rt_eval dicR emptyenv;;

(* Accesso ad elementi *)
rt_eval (Select(dic, "Nome")) emptyenv;;
rt_eval (Select(dic, "Cognome")) emptyenv;;
rt_eval (Select(dic, "Matricola")) emptyenv;;
rt_eval (Select(dic, "Eta")) emptyenv;;
rt_eval (Select(dic, "Anno Iscrizione")) emptyenv;;
rt_eval (Select(dic, "Residenza")) emptyenv;;
rt_eval (Select(dic, "ASDF")) emptyenv;;

(* Inserimento *)
rt_eval (Insert(dic, "ASDF", EBool(true))) emptyenv;;
rt_eval (Insert(dic, "Eta", EInt(24))) emptyenv;;

(* Rimozione *)
rt_eval (Remove(dic, "Eta")) emptyenv;;
rt_eval (Remove(dic, "Nome")) emptyenv;;
rt_eval (Remove(dic, "ASDF")) emptyenv;;

(* Clear *)
rt_eval (Clear(dic)) emptyenv;;

let dic1 = Dict(KeyValue("Matricola", EInt(547462), 
               KeyValue("Eta", Let("y", LetRec("fact", "n",
                                              IfThenElse(Equal(Den("n"), EInt(0)), 
                                              EInt(1),
                                              Prod(Den("n"), FunCall(Den("fact"), Sub(Den("n"), EInt(1))))), 
                                              FunCall(Den("fact"), EInt(4))),
                                  Sub(Den("y"), EInt(3))),
               KeyValue("Anno Iscrizione", EInt(2016),
               Unbound))));;
rt_eval dic1 emptyenv;;

(* ApplyOver *)
rt_eval (ApplyOver(Fun("x", Sum(Den("x"), EInt(1))), dic)) emptyenv;;
rt_eval (Let("y", EInt(2),
      Let("somma", Fun("x", Sum(Den("x"), Den("y"))),
      Let("y", EInt(5), ApplyOver(Den("somma"), dic1))))) emptyenv;;

