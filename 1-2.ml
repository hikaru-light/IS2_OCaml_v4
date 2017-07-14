module Parser = struct

module L = Lexer
(* module E = Evaluator *)

let revTok = ref ([] : L.token list)
let getToken () = match !revTok with
                   [] -> L.gettoken ()
                  | h::tl -> (revTok := tl; h)
(*if (!revTok = []) then L.gettoken ()
                  else let (h::tl) = !revTok in (revTok := tl; h)*)
let tok = ref (L.ONE ' ')
let revToken t = (revTok := (!tok)::(!revTok); tok := t)
let advance () = (tok := getToken(); L.print_token (!tok))

exception Syntax_error
let error () = raise Syntax_error
let check t = match !tok with
      L.CID _ -> if (t = (L.CID "")) then () else error()
    | L.VID _ -> if (t = (L.VID "")) then () else error()
    | L.NUM _ -> if (t = (L.NUM "")) then () else error()
    | tk      -> if (tk=t) then () else error()

let eat t = (check t; advance())

let rec clauses() = match !tok with
                  L.EOF -> ()
                | _ -> (clause(); clauses())
and clause() = match !tok with
                  L.ONE '('  -> (term(); eat(L.ONE '.'))
                | _ -> (predicate(); to_opt(); eat(L.ONE '.'))
and to_opt() = match !tok with
               L.TO -> (eat(L.TO); terms())
             | L.ONE '.' -> ()
and command() = match !tok with
                  L.QUIT -> exit 0
                | L.OPEN -> (eat(L.OPEN);
                     match !tok with
                        L.CID s -> (eat(L.CID ""); check (L.ONE '.');
                            L._ISTREAM := open_in (s^".pl");
                            advance(); clauses(); close_in (!L._ISTREAM))
                     | _ -> error())
                | _ -> (term(); check(L.ONE '.'))
and term() = match !tok with
             L.ONE '(' -> term()
           | _ -> predicate()
and terms() = term(); terms'()
and terms'() = match !tok with
                 L.ONE ',' -> (eat(L.ONE ','); term(); terms'())
                 | _ -> term()
and predicate() = funname(); eat(L.ONE '('); args(); eat(L.ONE ')')
and args() = (expr(); args'())
and args'() = match !tok with
                L.ONE ',' -> (eat(L.ONE ','); expr(); args'())
                | _ -> expr()
and expr() = match !tok with
                  L.ONE '(' -> expr_non_term()
                | L.ONE '[' -> expr_non_term()
                | L.VID _ -> id()
                | L.NUM _ -> id()
                | L.CID s  -> (try term() with
                          Syntax_error -> (revToken (L.CID s); id()))
                | _ -> term()
and expr_non_term() = match !tok with
                      L.ONE '(' -> (expr_non_term(); eat(L.ONE ')'))
                    | L.ONE '[' -> (list(); eat(L.ONE ']')) 
and list() = match !tok with
                  L.ONE ']' -> ()
                | _ -> (expr(); list_opt())
and list_opt() = match !tok with
                  L.ONE '|' -> (eat(L.ONE '|'); id())
                | L.ONE ',' -> (eat(L.ONE ','); list())
                | _ -> ()
and id() = match !tok with
             L.CID _ -> eat(L.CID "")
           | L.VID _ -> eat(L.VID "")
           | L.NUM _ -> eat(L.NUM "")
and funname() = eat(L.CID "")
end

let rec run() =
     print_string "?- ";
     while true do
         flush stdout; Lexer._ISTREAM := stdin;
         Parser.advance(); Parser.command(); print_string "\n?- "
done
