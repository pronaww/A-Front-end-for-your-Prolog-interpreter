%{
  open List
  open BeAst
%}

%token <string> Var
%token <string> String
%token TRUE FALSE
%token LPAREN RPAREN
%token TURNSTILE
%token <int> Int
%token <float> Float
%token PLUS, MINUS, TIMES, DIV, MOD, EXPO
%token EQUAL, GTR, LSR, GTE, LTE
%token COMMA, SEMICOLON, PERIOD
%token LSQR, RSQR, BAR
%token CUT, FAIL
%token EOF

%start e
%type <BeAst.program> e

%%

e: s LPAREN l RPAREN PERIOD e {[Fact(Symbol($1, $3))]@$6}
	| s LPAREN l RPAREN COMMA e {[Fact(Symbol($1, $3))]@$6}
	| CUT PERIOD {[Fact(CUT)]}
	| CUT COMMA e {[Fact(CUT)]@$3}
	| s LPAREN l RPAREN TURNSTILE sub e {[Rule(Symbol($1, $3),Subgoals($6))]@$7}
	| /*epsilon*/ { [] }

sub: s LPAREN l RPAREN COMMA sub {[(Symbol($1, $3))]@$6}
	| s LPAREN l RPAREN PERIOD {[(Symbol($1, $3))]}


s: String {$1}
	| Var {$1}

l: c_v COMMA l {[$1]@$3}
	| c_v {[$1]}
	| /*epsilon*/ { [] }

c_v: String {Const(String $1)}
	| Var {Var($1)}
	| n1 {Const($1)}

n1: n MINUS n1 {(Minus($1, $3))}
	| n {$1}

n: t PLUS n {(Plus($1, $3))}
	| t {$1}

t: f3 TIMES t {Times($1, $3)}
	| f3 {$1}

f3: f1 MOD f3 {Mod($1, $3)}
	| f1 {$1}

f1: f2 DIV f1 {Div($1, $3)}
	| f2 {$1}

f2: f4 EXPO f2 {Expo($1, $3)}
	| f4 {$1}

f4: MINUS f {Minus(Int(0),$2)}
	| f {$1}

f: Int {(Int($1))}
	| Float {(Float($1))}