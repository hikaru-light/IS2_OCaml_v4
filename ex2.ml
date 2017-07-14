type ast = Atom of string | Var of string | App of string * ast list

module P = Printf
let rec print_ast ast = match ast with
(App(s, hd::tl)) -> (P.printf "App(\"%s\",[" s;
print_ast hd; List.iter (fun x -> (print_string ";"; print_ast x)) tl;
print_string "])")
| (App(s, [])) -> P.printf "App(\"%s\",[])" s
| (Atom s) -> P.printf "Atom\"%s\"" s
| (Var s) ->  P.printf "Var\"%s\"" s

let print_ast_list lst = match lst with
(hd::tl) -> (print_string "["; print_ast hd;
List.iter (fun x -> (print_string ";";
print_ast x)) tl; print_string "]")
|  [] -> print_string "[]"

let sub name term =
let rec mapVar ast = match ast with
(Atom x) -> Atom(x)
| (Var n) -> if n=name then term else Var n
| (App(n, terms)) -> App(n, List.map mapVar terms)
in mapVar

let mgu (a,b) =
let rec ut (one, another, unifier) = match (one, another) with
([], []) -> (true, unifier)
| (term::t1, Var(name)::t2) ->
let r = fun x -> sub name term (unifier x) in
ut(List.map r t1, List.map r t2, r)
| (Var(name)::t1, term::t2) ->
let r = fun x -> sub name term (unifier x) in
ut(List.map r t1, List.map r t2, r)
| (Atom(n)::t1, Atom(m)::t2) ->
if n=m then ut(t1,t2,unifier) else (false, unifier)
| (App(n1,xt1)::t1, App(n2,xt2)::t2) ->
if n1=n2 && List.length xt1 = List.length xt2 then
ut(xt1@t1, xt2@t2, unifier)
else (false, unifier)
| (_,_) -> (false, unifier);
in ut ([a],[b], (fun x -> x))

let resolution (rule, question) = match (rule, question) with
(head::conds,goal::goals) ->
let (unifiable, unifier) = mgu (head,goal)
in  List.map unifier (conds@goals)
| ([], goals) -> goals
| (_, []) -> []

exception Compiler_error
let rec solve (program, question, result) = match question with
[] -> succeed result
| goal::goals ->
let onestep _ clause = match clause with
[]
-> raise Compiler_error
| head::conds  ->
let (unifiable, unifier) = mgu(head,goal) in
if unifiable then
solve (program,List.map unifier (conds@goals),unifier result)
else true
in List.fold_left onestep true program
let eval (program, question) = solve(program, [question], question)

let succeed query = (print_ast query; true)
