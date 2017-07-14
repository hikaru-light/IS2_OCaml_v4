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
