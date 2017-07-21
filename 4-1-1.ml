module Lexer = struct

module P = Printf
exception End_of_system
let _ISTREAM = ref stdin
let ch = ref []
let read () = match !ch with [] -> input_char !_ISTREAM
                      | h::rest -> (ch := rest; h)
let unread c = ch := c::!ch
let lookahead () = try let c = read () in unread c; c with End_of_file -> '$'

type token = CID of string | VID of string | NUM of string
| TO | IS | QUIT | OPEN | EOF | ONE of char

let rec integer i =
  let  c = lookahead () in
    if (c >= '0' && c <= '9') then
      identifier (i^(Char.escaped (read ())))
    else i

and identifier id =
  let  c = lookahead () in
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') || c == '_') then
        identifier (id^(Char.escaped (read ())))
    else id

and native_token () =
  let c = lookahead () in
    if (c >= 'a' && c <= 'z') then CID (identifier "")
    else if (c >= 'A' && c <= 'Z') then VID (identifier "")
    else if (c >= '0' && c <= '9') then NUM (integer "")
    else if (c =':') then (read();
           let d = lookahead () in if (d='-') then (read(); TO)
                                   else native_token ())
    else ONE (read ())

and gettoken () =
  try
    let token = native_token () in
      match token with
        ONE ' '  -> gettoken ()
      | ONE '\t' -> gettoken ()
      | ONE '\n' -> gettoken ()
      | _ -> token
  with End_of_file -> EOF

let print_token tk =
match tk with
(CID i) -> P.printf "CID(%s)" i
| (VID i) -> P.printf "VID(%s)" i
| (NUM i) -> P.printf "NUM(%s)" i
| (TO) -> P.printf ":-"
| (QUIT) -> P.printf "quit"
| (OPEN) -> P.printf "open"
| (IS) -> P.printf "is"
| (EOF) -> P.printf "eof"
| (ONE c) -> P.printf "ONE(%c)" c

let rec run () =
  flush stdout;
  let rlt = gettoken () in
    match rlt with
      (ONE '$') -> raise End_of_system
      | _       -> (print_token rlt; P.printf "\n"; run())

end

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
