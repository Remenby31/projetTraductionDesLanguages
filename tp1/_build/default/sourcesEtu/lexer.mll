{
type token = ID of string | AUTO  | IFACE | INET | STATIC | IP | ADRESS | NETMASK | GATEWAY | LOOPBACK | DHCP | EOF

exception Error of string
}


(* Définitions de macro pour les expressions régulières *)
let blanc = [' ' '\t' '\n']
let nb_ip = "25" ['0'-'5'] | '2' ['0'-'4'] ['0'-'9'] | '1' ['0'-'9'] | ['0'-'9'] | ['1'-'9'] ['0'-'9'] | ['0'-'9']
let ip = nb_ip '.' nb_ip '.' nb_ip '.' nb_ip
let nomInterface = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

 (* À compléter éventuellement *)


(* Règles léxicales *)
rule interface = parse
|  blanc (* On ignore les blancs *)
    { interface lexbuf }
| "auto"
    { AUTO }
| "iface"
    { IFACE }
| "inet"
    { INET }
| "static"
    { STATIC }
| "address"
    { ADRESS }
| "netmask"
    { NETMASK }
| "gateway"
    { GATEWAY }
| "loopback"
    { LOOPBACK }
| "dhcp"
    { DHCP }
| ip
    { IP }
| nomInterface as n
    { ID n }
| eof
    { EOF }
| _
{ raise (Error ("Unexpected char: "^(Lexing.lexeme lexbuf)^" at "^(string_of_int (Lexing.lexeme_start
lexbuf))^"-"^(string_of_int (Lexing.lexeme_end lexbuf)))) }
