// Long term, I would like to put a Lexical-Analyzer Generator in here.
//
// Nonetheless, I think we are happy with our hand-coded lexer.rs, and
// I should strive to remain compatible with it, both for short-term
// and perhaps also for long term.

mod grammar {
    use std::str;
    use std::cmp;

    trait Terminal { }
    trait NonTerminal { }

    enum Sym<T,NT> { T(T), NT(NT) }

    struct Production<T, NT> {
        head: NT,
        body: ~[Sym<T, NT>],
    }

    struct Grammar<T, NT> {
        start: NT,
        productions: ~[Production<T,NT>],
    }

    trait GenFresh {
        fn prime(&self) -> Self;
        fn gensym() -> Self;
    }

    impl<T:ToStr,NT:ToStr> ToStr for Sym<T,NT> {
        fn to_str(&self) -> ~str {
            match *self {
                T(ref t)   => ~"\"" + t.to_str() + "\"",
                NT(ref nt) => ~"<"  + nt.to_str() + ">",
            }
        }
    }

    fn fill_left(s:~str, c:char, width:uint) -> ~str {
        if s.char_len() < width {
            str::from_char(c).repeat(width - s.char_len()) + s
        } else {
            s
        }
    }

    impl<T:ToStr,NT:ToStr> ToStr for Production<T,NT> {
        fn to_str(&self) -> ~str {
            self.to_str_head_aligned(0)
        }
    }

    impl<T:ToStr,NT:ToStr> Production<T,NT> {
        fn to_str_head_aligned(&self, width:uint) -> ~str {
            let head = self.head.to_str();
            let head = fill_left("<"+head+">", ' ', width+2);
            head + " ::= " + self.body.map(|x|x.to_str()).connect(" ")
        }
    }

    impl<T:ToStr,NT:ToStr> ToStr for Grammar<T,NT> {
        fn to_str(&self) -> ~str {
            let w = self.productions.iter().map(|x|x.head.to_str().char_len()).fold(0u, cmp::max);
            self.productions.map(|x|x.to_str_head_aligned(w)).connect("\n")
        }
    }

    fn production<T,NT>(h:NT, b: ~[Sym<T,NT>]) -> Production<T,NT> {
        Production { head:h, body:b }
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

    fn ex_elim_amb_1() -> StaticGrammar { Grammar {
        start: "stmt",
        productions: ~[
            production!( stmt -> T:"if" N:expr T:"then" N:stmt                 ),
            production!( stmt -> T:"if" N:expr T:"then" N:stmt T:"else" N:stmt ),
            production!( stmt -> T:"other"                                     ),
        ]}}

    fn ex_elim_amb_2() -> StaticGrammar { Grammar {
        start: "stmt",
        productions: ~[
            production!( stmt           -> N:matched_stmt                          ),
            production!( stmt           -> N:unmatched_stmt                        ),
            production!( matched_stmt   -> T:"if" N:expr
                                           T:"then" N:matched_stmt
                                           T:"else" N:matched_stmt                 ),
            production!( matched_stmt   -> T:"other"                               ),
            production!( unmatched_stmt -> T:"if" N:expr T:"then" N:stmt           ),
            production!( unmatched_stmt -> T:"if" N:expr T:"then" N:matched_stmt
                                                         T:"else" N:unmatched_stmt ),
       ]}}

    fn ex_left_recur_1() -> StaticGrammar { Grammar {
        start: "E",
        productions: ~[
            production!( E -> N:E T:"T" N:T   ),
            production!( E -> N:T             ),
            production!( T -> N:T T:"*" N:F   ),
            production!( T -> N:F             ),
            production!( F -> T:"(" N:E T:")" ),
            production!( F -> T:"id"          ),
        ]}}

    fn ex_left_recur_2() -> StaticGrammar { Grammar {
        start: "E",
        productions: ~[
            production!(  E -> N:T N:E2        ),
            production!( E2 -> T:"+" N:T N:E2  ),
            production!(  T -> N:F N:T2        ),
            production!( T2 -> T:"*" N:F N:T2  ),
            production!( T2 ->                 ),
            production!(  F -> T:"(" N:E T:")" ),
            production!(  F -> T:"id"          ),
        ]}}

    // Eliminating immediate left recursion for A is the transformation of
    //
    //    A -> A \alpha_1 | A \alpha_2 | ... | A \alpha_m
    //       | \beta_1 | \beta_2 | ... | \beta_n
    //
    // where no \beta_i begins with an A (and no \alpha_i is \epsilon),
    // replacing the A-productions by:
    //
    //    A -> \beta_1 A2 | \beta_2 A2 | ... | \beta_n A2
    //   A2 -> \alpha_1 A2 | \alpha_2 A2 | ... \alpha_m A2 | \epsilon
    //
    // where A2 is fresh.


    // Eliminating all (immediate and multi-step) left recursion from a
    // grammar, for grammars with no cycles or \epsilon-productions.
    // (Note that the resulting grammar may have \epsilon productions.)
    //
    // Let the nonterminals be A_1, A_2, ..., A_n
    //
    // for i := 1 to n do:
    //   for j := 1 to i-1 do:
    //     replace each production of the form A_i -> A_j \gamma
    //       by the productions A_i -> \delta_1 \gamma | \delta_2 \gamma | ... | \delta_k \gamma,
    //       where A_j -> \delta_1 | \delta_2 | ... | \delta_k are all the current A_j-productions;
    //   end
    //   eliminate the immediate left recursion among the A_i productions
    // end

    #[test]
    fn whoa() {
        let ex4_5 = example_4_5();
        println(fmt!("%s\n", ex4_5.to_str()));
        println(fmt!("%s\n", example_4_6().to_str()));
        println(fmt!("%s\n", example_4_7().to_str()));
        println(fmt!("%s\n", example_4_13().to_str()));
        println(fmt!("%s\n", ex_elim_amb_1().to_str()));
        println(fmt!("%s\n", ex_left_recur_1().to_str()));
        println(fmt!("%s\n", ex_left_recur_2().to_str()));
    }
}

fn main() {
    println("Hello World");
}
