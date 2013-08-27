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

    #[deriving(Clone)]
    enum ProductionSym<T,NT> { T(T), NT(NT) }

    #[deriving(Clone)]
    struct Prod<T, NT> {
        head: NT,
        body: ~[ProductionSym<T, NT>],
    }

    struct Grammar<T, NT> {
        start: NT,
        productions: ~[Prod<T,NT>],
    }

    trait Primable {
        fn prime(&self) -> Self;
    }

    trait GenFresh<Registry> : Primable {
        fn gensym(&mut Registry) -> Self;
    }

    #[deriving(Eq,Clone)]
    enum SymVariant<T> { core(T), gensym(T, uint) }

    #[deriving(Eq)]
    struct SymbolRegistry(@mut uint);

    fn new_symbol_registry() -> SymbolRegistry {
        SymbolRegistry(@mut 0)
    }

    #[deriving(Eq)]
    struct Sym<T> {
        registry: SymbolRegistry,
        value: SymVariant<T>
    }

    fn sym<T>(registry: SymbolRegistry, value: SymVariant<T>) -> Sym<T> {
        Sym{ registry: registry, value: value }
    }

    impl<T:Clone> Clone for Sym<T> {
        fn clone(&self) -> Sym<T> {
            sym(self.registry, self.value.clone())
        }
    }

    impl<T:Clone> Primable for Sym<T> {
        fn prime(&self) -> Sym<T> {
            let g = self.registry;
            let count : uint = **g;
            let count = count + 1;
            **self.registry = count;
            let t = match self.value { core(ref t) => t, gensym(ref t,_) => t };
            sym(self.registry, gensym(t.clone(), count))
        }
    }

    impl<T:ToStr> ToStr for SymVariant<T> {
        fn to_str(&self) -> ~str {
            match self {
                &core(ref t) => t.to_str(),
                &gensym(ref t, count) => t.to_str() + "_" + count.to_str()
            }
        }
    }

    impl<T:ToStr> ToStr for Sym<T> {
        fn to_str(&self) -> ~str { self.value.to_str() }
    }

    impl<T:ToStr,NT:ToStr> ToStr for ProductionSym<T,NT> {
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

    impl<T:ToStr,NT:ToStr> ToStr for Prod<T,NT> {
        fn to_str(&self) -> ~str {
            self.to_str_head_aligned(0)
        }
    }

    impl<T:ToStr,NT:ToStr> Prod<T,NT> {
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

    fn production<T,NT>(h:NT, b: ~[ProductionSym<T,NT>]) -> Prod<T,NT> {
        Prod { head:h, body:b }
    }

    // The $G argument is the SymbolRegistry.  (Originally I used a
    // tricky of overloading the N to act as a tag and also a binding
    // for the registry, but was not sufficient since the head of the
    // production needs the registry (even when the right hand side
    // has no non-terminals occurrences).

    macro_rules! production (
        ($G:ident $H:ident -> $($T:ident : $S:expr)*) => (
            production(sym($G, core(stringify!($H))), ~[$(symbolify!($G $T $S)),*])
        )
    )

    macro_rules! symbolify (
        (       $G:ident N $NT:expr) => ( NT(sym($G, core(stringify!($NT)))) );
        ( $ignored:ident T $T:expr)  => (  T($T)                             );
    )

    type StaticGrammar = Grammar<&'static str, Sym<&'static str>>;

    fn example_4_5() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("expression")),
        productions: ~[
            production!( G expression -> N:expression T:"+" N:term ),
            production!( G expression -> N:expression T:"-" N:term ),
            production!( G expression -> N:term                    ),
            production!( G       term -> N:term T:"*" N:factor     ),
            production!( G       term -> N:term T:"/" N:factor     ),
            production!( G       term -> N:factor                  ),
            production!( G     factor -> T:"(" N:expression T:")"  ),
            production!( G     factor -> T:"id"                    ),
        ]}}

    fn example_4_6() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G E -> N:E T:"+" N:T   ), // E -> E + T | E - T | T
            production!( G E -> N:E T:"-" N:T   ),
            production!( G E -> N:T             ),

            production!( G T -> N:T T:"*" N:F   ), // T -> T * F | T / F | F
            production!( G T -> N:T T:"/" N:F   ),

            production!( G F -> T:"(" N:E T:")" ), // F -> ( E ) | id
            production!( G F -> T:"id"          ),
        ]}}

    fn example_4_7() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G E -> N:E T:"+" N:E   ), // E -> E + E | E * E | - E | ( E ) | id
            production!( G E -> N:E T:"*" N:E   ),
            production!( G E -> T:"-" N:E       ),
            production!( G E -> T:"(" N:E T:")" ),
            production!( G E -> T:"id"          ),
        ]}}

    fn example_4_13() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("S")),
        productions: ~[
            production!( G S -> T:"(" N:S T:")" ), // S -> ( S ) S | \epsilon
            production!( G S -> ),
        ]}}

    fn exercise_4_2_1() -> StaticGrammar { let _ = "aa+a*";
        let G = new_symbol_registry(); Grammar {
        start: sym(G, core("S")),
        productions: ~[
            production!( G S -> N:S N:S T:"+" ), // S -> S S + | S S * | a
            production!( G S -> N:S N:S T:"*" ),
            production!( G S -> T:"a"         ),
        ]}}

    fn exercise_4_2_2_help(input: &'static str,
                           n: SymbolRegistry,
                           prods: ~[Prod<&'static str, Sym<&'static str>>]) -> StaticGrammar {
        let _ = input;
        Grammar{ start: sym(n, core("S")), productions: prods }
    }

    fn exercise_4_2_2_a() -> StaticGrammar {
        let G = new_symbol_registry();
        exercise_4_2_2_help("000111", G, ~[
            production!( G S -> T:"0" N:S T:"1" ), // S -> 0 S 1 | 0 1
            production!( G S -> T:"0" T:"1"     ),
        ])}

    fn exercise_4_2_2_b() -> StaticGrammar {
        let G = new_symbol_registry();
        exercise_4_2_2_help("+*aaa", G, ~[
            production!( G S -> T:"+" N:S N:S ), // S -> + S S | * S S | a
            production!( G S -> T:"*" N:S N:S ),
            production!( G S -> T:"a"         ),
        ])}

    fn exercise_4_2_2_c() -> StaticGrammar {
        let G = new_symbol_registry();
        exercise_4_2_2_help("(()())", G, ~[
            production!( G S -> N:S T:"(" N:S T:")" N:S ), // S -> S ( S ) S | \epsilon
            production!( G S ->                         ),
        ])}

    fn ex_elim_amb_1() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("stmt")),
        productions: ~[
            production!( G stmt -> T:"if" N:expr T:"then" N:stmt                 ),
            production!( G stmt -> T:"if" N:expr T:"then" N:stmt T:"else" N:stmt ),
            production!( G stmt -> T:"other"                                     ),
        ]}}

    fn ex_elim_amb_2() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("stmt")),
        productions: ~[
            production!( G stmt           -> N:matched_stmt                          ),
            production!( G stmt           -> N:unmatched_stmt                        ),
            production!( G matched_stmt   -> T:"if" N:expr
                                             T:"then" N:matched_stmt
                                             T:"else" N:matched_stmt                 ),
            production!( G matched_stmt   -> T:"other"                               ),
            production!( G unmatched_stmt -> T:"if" N:expr T:"then" N:stmt           ),
            production!( G unmatched_stmt -> T:"if" N:expr T:"then" N:matched_stmt
                                                           T:"else" N:unmatched_stmt ),
       ]}}

    fn ex_left_recur_1() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G E -> N:E T:"T" N:T   ),
            production!( G E -> N:T             ),
            production!( G T -> N:T T:"*" N:F   ),
            production!( G T -> N:F             ),
            production!( G F -> T:"(" N:E T:")" ),
            production!( G F -> T:"id"          ),
        ]}}

    fn ex_left_recur_2() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G  E -> N:T N:E2        ),
            production!( G E2 -> T:"+" N:T N:E2  ),
            production!( G  T -> N:F N:T2        ),
            production!( G T2 -> T:"*" N:F N:T2  ),
            production!( G T2 ->                 ),
            production!( G  F -> T:"(" N:E T:")" ),
            production!( G  F -> T:"id"          ),
        ]}}

    fn eliminate_immediate_left_recursion<T:Clone,NT:Eq+Clone+Primable>(prods:&[Prod<T, NT>])
        -> ~[Prod<T,NT>] {

        type P = Prod<T,NT>;
        type PS = ProductionSym<T,NT>;

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

        let mut accum : ~[P] = ~[];
        if prods.len() > 0 {
            let a = &prods[0].head;
            let bodies : ~[~[PS]] = prods.map(|p| { assert!(p.head == *a); p.body.clone() });
            let (alphas, betas) : (~[~[PS]], ~[~[PS]]) =
                bodies.partitioned(|b|
                                   b.len() > 0 &&
                                   match b[0] { T(_) => false, NT(ref s) => a == s });
            let alphas = alphas.map(|b| { assert!(b.len() > 1); b.slice_from(1).to_owned() });

            let a2 = a.prime();
            let new_beta_bodies  : ~[~[PS]] = betas.map(|b| {
                    let mut b = b.clone(); b.push(NT(a2.clone())); b });
            let mut new_alpha_bodies : ~[~[PS]] = alphas.map(|b| {
                    let mut b = b.clone(); b.push(NT(a2.clone())); b });
            new_alpha_bodies.push(~[]);

            for b_body in new_beta_bodies.iter() {
                accum.push(production(a.clone(), b_body.clone())); }
            for a_body in new_alpha_bodies.iter() {
                accum.push(production(a2.clone(), a_body.clone())) }
        }
        accum
    }

    impl<T,NT:Eq> Prod<T,NT> {
        fn is_empty(&self) -> bool { self.body.len() == 0 }
        fn is_left_recursive(&self) -> bool {
            self.body.len() >= 1 && match self.body[0] {
                T(_) => false, NT(ref h) => &self.head == h
            }
        }
        fn is_trivial_cycle(&self) -> bool {
            self.body.len() == 1 && self.is_left_recursive()
        }
    }

    fn has_empty<T,NT:Eq>(prods:&[Prod<T,NT>]) -> bool {
        prods.iter().any(|p| p.is_empty() )
    }

    fn has_left_recursion<T,NT:Eq>(prods:&[Prod<T, NT>]) -> bool {
        prods.iter().any(|p| p.is_left_recursive() )
    }


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
        println(fmt!("%s\n", eliminate_immediate_left_recursion(ex_elim_amb_1().productions).to_str()));
    }
}

fn main() {
    println("Hello World");
}
