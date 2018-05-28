(* type exp = Int of int | Float of float | Symbolic_const of string | String of string | Bool of bool

type term = Var of string | Const of exp | Func of string * (term list)

type atomic_formula = Symbol of string * term list | FAIL | CUT

type goal = Sequence of atomic_formula list

type head = atomic_formula

type body = Subgoals of atomic_formula list

type clause = Fact of head | Rule of head * body

type program = clause list

 *)(***************************************************************************)
open BeAst;;
open List;;

let parse_string str =
	let lb = Lexing.from_string str
	in
		BeParser.e BeLexer.token lb
		
let token_list_of_string s =
	let lb = Lexing.from_string s in
	let rec helper l =
		try
			let t = BeLexer.token lb in
			if (t = BeParser.EOF) then List.rev l else helper (t::l)
		with _ -> List.rev l
	in 
		helper []

let rec prinunif l' = match l' with
							[] -> []
| (Var v, Const(String s))::xs -> Printf.printf "%s = %s" v s; if (length l')=1 then () else Printf.printf ",\n"; prinunif xs

let rec prin l = match l with
	(Unification l')::tl -> (prinunif l'); prin tl
	| [NEXT] -> print_string ".\n\n"
	| (NEXT)::tl -> print_string " ;\n"; prin tl
	| true::tl -> print_string "true"; prin tl
	| false::tl -> print_string "false. \n\n"; prin tl
	| [] -> print_string ""

let _ =
          try
          	let program_channel = Scanf.scanf "%s" (open_in) in
            let lexbuf = Lexing.from_channel program_channel in
            let program = BeParser.e BeLexer.token lexbuf in
            let lexbuf = Lexing.from_channel stdin in
            while true do
              Printf.printf "?- ";
              let query = parse_string (read_line ()) (* BeParser.e BeLexer.token lexbuf *) in (* Parser.e Lexer.token lexbuf in *)
                let rec matcher q = match q with
                	Fact(s)::xs -> s::(matcher xs)
                	| [] -> [] (* (matcher (Scanf.scanf "%s" (parse_string))) *)in
                flush stdout; prin (eval(program, program, [], matcher query, [], [])); flush stdout
            done
          with BeLexer.HALT ->
            exit 0