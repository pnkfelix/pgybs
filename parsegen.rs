// Long term, I would like to put a Lexical-Analyzer Generator in here.
//
// Nonetheless, I think we are happy with our hand-coded lexer.rs, and
// I should strive to remain compatible with it, both for short-term
// and perhaps also for long term.

mod grammar {
    trait Terminal { }
    trait NonTerminal { }

    enum Sym<T,NT> { T(T), NT(NT) }

    struct Production<T, NT> {
        head: NT,
        body: ~[Sym<T, NT>],
    }

    impl<T:ToStr,NT:ToStr> ToStr for Sym<T,NT> {
        fn to_str(&self) -> ~str {
            match *self {
                T(ref t)   => ~"\"" + t.to_str() + "\"",
                NT(ref nt) => ~"<"  + nt.to_str() + ">",
            }
        }
    }

    impl<T:ToStr,NT:ToStr> ToStr for Production<T,NT> {
        fn to_str(&self) -> ~str {
            ~"<" + self.head.to_str() + "> ::= " + self.body.map(|x|x.to_str()).connect(" ")
        }
    }

    impl<T:ToStr,NT:ToStr> ToStr for Grammar<T,NT> {
        fn to_str(&self) -> ~str {
            self.productions.map(|x|x.to_str()).connect("\n")
        }
    }

    fn production<T,NT>(h:NT, b: ~[Sym<T,NT>]) -> Production<T,NT> {
        Production { head:h, body:b }
    }

    struct Grammar<T, NT> {
        start: NT,
        productions: ~[Production<T,NT>],
    }

    macro_rules! production (
        ($H:ident -> $($T:ident : $S:expr)*) => (
            production(stringify!($H), ~[$(symbolify!($T $S)),*])
        )
    )

    macro_rules! symbolify (
        (N $N:expr) => ( NT(stringify!($N)) );
        (T $T:expr) => ( T($T)              );
    )

    type StaticGrammar = Grammar<&'static str,&'static str>;

    fn example_4_5() -> StaticGrammar { Grammar {
        start: "expression",
        productions: ~[
            production!( expression -> N:expression T:"+" N:term ),
            production!( expression -> N:expression T:"-" N:term ),
            production!( expression -> N:term                    ),
            production!(       term -> N:term T:"*" N:factor     ),
            production!(       term -> N:term T:"/" N:factor     ),
            production!(       term -> N:factor                  ),
            production!(     factor -> T:"(" N:expression T:")"  ),
            production!(     factor -> T:"id"                    ),
        ]}}

    fn example_4_6() -> StaticGrammar { Grammar {
        start: "E",
        productions: ~[
            production!( E -> N:E T:"+" N:T   ), // E -> E + T | E - T | T
            production!( E -> N:E T:"-" N:T   ),
            production!( E -> N:T             ),

            production!( T -> N:T T:"*" N:F   ), // T -> T * F | T / F | F
            production!( T -> N:T T:"/" N:F   ),

            production!( F -> T:"(" N:E T:")" ), // F -> ( E ) | id
            production!( F -> T:"id"          ),
        ]}}

    fn example_4_7() -> StaticGrammar { Grammar {
        start: "E",
        productions: ~[
            production!( E -> N:E T:"+" N:E   ), // E -> E + E | E * E | - E | ( E ) | id
            production!( E -> N:E T:"*" N:E   ),
            production!( E -> T:"-" N:E       ),
            production!( E -> T:"(" N:E T:")" ),
            production!( E -> T:"id"          ),
        ]}}

    fn example_4_13() -> StaticGrammar { Grammar {
        start: "S",
        productions: ~[
            production!( S -> T:"(" N:S T:")" ), // S -> ( S ) S | \epsilon
            production!( S -> ),
        ]}}

    fn exercise_4_2_1() -> StaticGrammar { let _ = "aa+a*"; Grammar {
        start: "S",
        productions: ~[
            production!( S -> N:S N:S T:"+" ), // S -> S S + | S S * | a
            production!( S -> N:S N:S T:"*" ),
            production!( S -> T:"a"         ),
        ]}}

    fn exercise_4_2_2_help(input: &'static str,
                           prods: ~[Production<&'static str, &'static str>]) -> StaticGrammar {
        let _ = input; Grammar{ start: "S", productions: prods }
    }

    fn exercise_4_2_2_a() -> StaticGrammar {
        exercise_4_2_2_help("000111", ~[
            production!( S -> T:"0" N:S T:"1" ), // S -> 0 S 1 | 0 1
            production!( S -> T:"0" T:"1"     ),
        ])}

    fn exercise_4_2_2_b() -> StaticGrammar {
        exercise_4_2_2_help("+*aaa", ~[
            production!( S -> T:"+" N:S N:S ), // S -> + S S | * S S | a
            production!( S -> T:"*" N:S N:S ),
            production!( S -> T:"a"         ),
        ])}

    fn exercise_4_2_2_c() -> StaticGrammar {
        exercise_4_2_2_help("(()())", ~[
            production!( S -> N:S T:"(" N:S T:")" N:S ), // S -> S ( S ) S | \epsilon
            production!( S ->                         ),
        ])}

    #[test]
    fn whoa() {
        let ex4_5 = example_4_5();
        println(fmt!("%s\n", ex4_5.to_str()));
        println(fmt!("%s\n", example_4_6().to_str()));
        println(fmt!("%s\n", example_4_7().to_str()));
        println(fmt!("%s\n", example_4_13().to_str()));
    }
}

fn main() {
    println("Hello World");
}
