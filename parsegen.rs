// Long term, I would like to put a Lexical-Analyzer Generator in here.
//
// Nonetheless, I think we are happy with our hand-coded lexer.rs, and
// I should strive to remain compatible with it, both for short-term
// and perhaps also for long term.

extern mod extra;

#[allow(unused_imports)] // work around lint bug (warns about use extra::sort)
mod grammar {
    use std::str;
    use std::cmp;
    use std::hashmap::HashMap;
    use extra::treemap::TreeMap;
    use std::to_bytes;
    use std::hashmap::HashSet;
    use std::vec;

    trait Terminal : IterBytes+Eq+Clone { }
    trait NonTerminal : IterBytes+Eq+Clone { }

    impl Terminal for &'static str { }
    impl NonTerminal for &'static str { }

    #[deriving(Clone,Eq,IterBytes)]
    enum ProductionSym<T,NT> { T(T), NT(NT) }

    impl<T,NT:Eq> ProductionSym<T,NT> {
        fn matches_nt(&self, other:&NT) -> bool {
            match self { &T(_) => false, &NT(ref nt) => nt == other }
        }
    }

    impl<T:Ord,NT:Ord> Ord for ProductionSym<T,NT> {
        fn lt(&self, other: &ProductionSym<T,NT>) -> bool {
            match (self, other) {
                ( &T(_),       &NT(_))     => true,
                (&NT(_),       &T(_))      => false,
                ( &T(ref t1),  &T(ref t2)) => t1 <= t2,
                (&NT(ref n1), &NT(ref n2)) => n1 <= n2
            }
        }
    }

    fn maximal_common_prefix<T:Eq+Clone>(v1: &[T], v2: &[T]) -> ~[T] {
        let mut accum = ~[];
        for (a, b) in v1.iter().zip(v2.iter()) {
            if (a == b) {
                accum.push(a.clone())
            } else {
                break;
            }
        }
        accum
    }

    fn factor_suffix<'r, T:Eq+Clone>(alpha: &[T], vec: &'r [T]) -> Option<&'r [T]> {
        if vec.len() < alpha.len() {
            return None;
        }
        let prefix = vec.slice_to(alpha.len());
        let suffix = vec.slice_from(alpha.len());
        if prefix == alpha {
            return Some(suffix);
        } else {
            return None;
        }
    }

    #[deriving(Clone,IterBytes,Eq)]
    struct PString<T, NT>(~[ProductionSym<T, NT>]);

    impl<T:Eq+Clone,NT:Eq+Clone> PString<T,NT> {
        fn maximal_common_prefix(&self, other: &PString<T,NT>) -> ~[ProductionSym<T,NT>] {
            maximal_common_prefix(**self, **other)
        }
    }

    #[deriving(Clone,IterBytes,Eq)]
    struct Prod<T, NT> { head: NT, body: PString<T, NT> }

    struct Grammar<T, NT> {
        start: NT,
        productions: ~[Prod<T, NT>],
    }

    struct ProductionIterator<'self,T,NT>(vec::VecIterator<'self,Prod<T,NT>>);

    struct TerminalSet<T>(HashSet<T>);
    struct NonterminalSet<NT>(HashSet<NT>);
    struct SymbolSet<T,NT>(HashSet<ProductionSym<T,NT>>);

    impl<T:Clone+IterBytes+Eq,NT:Clone+IterBytes+Eq> Grammar<T,NT> {
        fn terminals(&self) -> TerminalSet<T> {
            let mut accum = HashSet::new();
            do self.for_each_sym |s| {
                match s {
                    &T(ref t) => { accum.insert(t.clone()); },
                    &NT(_) => {}
                }
            }
            TerminalSet(accum)
        }

        fn non_terminals(&self) -> NonterminalSet<NT> {
            let mut accum = HashSet::new();
            do self.for_each_sym |s| {
                match s {
                    &T(_) => {}
                    &NT(ref nt) => { accum.insert(nt.clone()); },
                }
            }
            NonterminalSet(accum)
        }

        fn symbols(&self) -> SymbolSet<T,NT> {
            let mut accum = HashSet::new();
            do self.for_each_sym |s| {
                accum.insert(s.clone());
            }
            SymbolSet(accum)
        }

        fn for_each_sym(&self, f: &fn (&ProductionSym<T,NT>) -> ()) {
            f(&NT(self.start.clone()));
            for p in self.productions_iter() {
                f(&NT(p.head.clone()));
                for s in p.body.iter() {
                    f(s);
                }
            }
        }

        fn productions_iter<'a>(&'a self) -> ProductionIterator<'a, T, NT> {
            ProductionIterator(self.productions.iter())
        }
    }

    trait ToGrammar<T, NT> {
        fn to_grammar<'a>(&'a self) -> &'a Grammar<T, NT>;
    }

    trait OwnedGrammar<T, NT> {
        fn owned_grammar(~self) -> ~Grammar<T, NT>;
    }

    trait Primable {
        fn prime(&self) -> Self;
    }

    trait GenFresh<Registry> : Primable {
        fn gensym(&mut Registry) -> Self;
    }

    #[deriving(Eq,Clone,IterBytes,Ord,TotalOrd,TotalEq)]
    enum SymVariant<T> { core(T), gensym(T, uint) }

    struct SymbolRegistry(@mut uint);

    impl ToStr for SymbolRegistry {
        fn to_str(&self) -> ~str {
            ~"SymbolRegistry{ counter: "+(**self).to_str()+" }"
        }
    }

    impl Eq for SymbolRegistry {
        fn eq(&self, other: &SymbolRegistry) -> bool {
            use std::managed;
            managed::mut_ptr_eq(**self, **other)
        }
    }

    impl TotalEq for SymbolRegistry {
        fn equals(&self, other: &SymbolRegistry) -> bool {
            use std::managed;
            managed::mut_ptr_eq(**self, **other)
        }
    }

    impl Ord for SymbolRegistry {
        fn lt(&self, other: &SymbolRegistry) -> bool {
            use std::ptr;
            let s = ptr::to_unsafe_ptr(**self);
            let t = ptr::to_unsafe_ptr(**other);
            (s as uint) < (t as uint)
        }
    }

    impl TotalOrd for SymbolRegistry {
        fn cmp(&self, other: &SymbolRegistry) -> Ordering {
            use std::ptr;
            let s = ptr::to_unsafe_ptr(**self);
            let t = ptr::to_unsafe_ptr(**other);
            (&(s as uint)).cmp(&(t as uint))
        }
    }

    fn new_symbol_registry() -> SymbolRegistry {
        SymbolRegistry(@mut 0)
    }

    impl SymbolRegistry {
        fn sym<T>(&self, v:T) -> Sym<T> { sym(*self, core(v)) }
    }

    #[deriving(Eq, Ord, TotalOrd, TotalEq)]
    struct Sym<T> {
        registry: SymbolRegistry,
        value: SymVariant<T>
    }

    impl<T:IterBytes+Eq+Clone> NonTerminal for Sym<T> { }

    impl<T:IterBytes> IterBytes for Sym<T> {
        // (don't include the registry in the hash)
        fn iter_bytes(&self, lsb0: bool, f: to_bytes::Cb) -> bool {
            self.value.iter_bytes(lsb0, f)
        }
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
            "Grammar ⟨ "+self.productions.map(|x|x.to_str_head_aligned(w)).connect("\n") + " ⟩"
        }
    }

    fn production<T,NT>(h:NT, b: ~[ProductionSym<T,NT>]) -> Prod<T,NT> {
        Prod { head:h, body:PString(b) }
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

    trait GrammarLike<T,NT> {
        fn start(&self) -> NT;
        fn productions<'a>(&'a self) -> &'a [Prod<T, NT>];
        fn owned_productions(~self) -> ~[Prod<T, NT>];
    }

    type StaticGrammar = (SymbolRegistry, Grammar<&'static str, Sym<&'static str>>);

    type StaticSym = ProductionSym<&'static str, Sym<&'static str>>;
    type StaticStr = PString<&'static str, Sym<&'static str>>;

    impl ToGrammar<&'static str, Sym<&'static str>> for StaticGrammar {
        fn to_grammar<'a>(&'a self) -> &'a Grammar<&'static str, Sym<&'static str>> {
            let &(_, ref g) = self; g
        }
    }

    impl OwnedGrammar<&'static str, Sym<&'static str>> for StaticGrammar {
        fn owned_grammar(~self) -> ~Grammar<&'static str, Sym<&'static str>> {
            let ~(_, g) = self; ~g
        }
    }

    impl GrammarLike<&'static str, Sym<&'static str>> for StaticGrammar {
        fn start(&self) -> Sym<&'static str> {
            self.to_grammar().start
        }
        fn productions<'a>(&'a self) -> &'a [Prod<&'static str, Sym<&'static str>>] {
            self.to_grammar().productions.as_slice()
        }
        fn owned_productions(~self) -> ~[Prod<&'static str, Sym<&'static str>>] {
            let ~(_, g) = self;
            g.productions
        }
    }

    fn example_4_5() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
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
        ]})}

    fn example_4_6() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G E -> N:E T:"+" N:T   ), // E -> E + T | E - T | T
            production!( G E -> N:E T:"-" N:T   ),
            production!( G E -> N:T             ),

            production!( G T -> N:T T:"*" N:F   ), // T -> T * F | T / F | F
            production!( G T -> N:T T:"/" N:F   ),

            production!( G F -> T:"(" N:E T:")" ), // F -> ( E ) | id
            production!( G F -> T:"id"          ),
        ]})}

    fn example_4_7() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G E -> N:E T:"+" N:E   ), // E -> E + E | E * E | - E | ( E ) | id
            production!( G E -> N:E T:"*" N:E   ),
            production!( G E -> T:"-" N:E       ),
            production!( G E -> T:"(" N:E T:")" ),
            production!( G E -> T:"id"          ),
        ]})}

    fn example_4_13() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("S")),
        productions: ~[
            production!( G S -> T:"(" N:S T:")" ), // S -> ( S ) S | \epsilon
            production!( G S -> ),
        ]})}

    fn exercise_4_2_1() -> ~StaticGrammar { let _ = "aa+a*";
        let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("S")),
        productions: ~[
            production!( G S -> N:S N:S T:"+" ), // S -> S S + | S S * | a
            production!( G S -> N:S N:S T:"*" ),
            production!( G S -> T:"a"         ),
        ]})}

    fn exercise_4_2_2_help(input: &'static str,
                           n: SymbolRegistry,
                           prods: ~[Prod<&'static str, Sym<&'static str>>]) -> ~StaticGrammar {
        let _ = input;
        ~(n,Grammar{ start: sym(n, core("S")), productions: prods })
    }

    fn exercise_4_2_2_a() -> ~StaticGrammar {
        let G = new_symbol_registry();
        exercise_4_2_2_help("000111", G, ~[
            production!( G S -> T:"0" N:S T:"1" ), // S -> 0 S 1 | 0 1
            production!( G S -> T:"0" T:"1"     ),
        ])}

    fn exercise_4_2_2_b() -> ~StaticGrammar {
        let G = new_symbol_registry();
        exercise_4_2_2_help("+*aaa", G, ~[
            production!( G S -> T:"+" N:S N:S ), // S -> + S S | * S S | a
            production!( G S -> T:"*" N:S N:S ),
            production!( G S -> T:"a"         ),
        ])}

    fn exercise_4_2_2_c() -> ~StaticGrammar {
        let G = new_symbol_registry();
        exercise_4_2_2_help("(()())", G, ~[
            production!( G S -> N:S T:"(" N:S T:")" N:S ), // S -> S ( S ) S | \epsilon
            production!( G S ->                         ),
        ])}

    fn ex_elim_amb_1() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("stmt")),
        productions: ~[
            production!( G stmt -> T:"if" N:expr T:"then" N:stmt                 ),
            production!( G stmt -> T:"if" N:expr T:"then" N:stmt T:"else" N:stmt ),
            production!( G stmt -> T:"other"                                     ),
        ]})}

    fn ex_elim_amb_2() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
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
       ]})}

    fn ex_left_recur_1() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G E -> N:E T:"T" N:T   ),
            production!( G E -> N:T             ),
            production!( G T -> N:T T:"*" N:F   ),
            production!( G T -> N:F             ),
            production!( G F -> T:"(" N:E T:")" ),
            production!( G F -> T:"id"          ),
        ]})}

    fn ex_left_recur_2() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("E")),
        productions: ~[
            production!( G  E -> N:T N:E2        ),
            production!( G E2 -> T:"+" N:T N:E2  ),
            production!( G  T -> N:F N:T2        ),
            production!( G T2 -> T:"*" N:F N:T2  ),
            production!( G T2 ->                 ),
            production!( G  F -> T:"(" N:E T:")" ),
            production!( G  F -> T:"id"          ),
        ]})}

    fn ex_left_factor_1() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("stmt")),
        productions: ~[
            production!( G  stmt -> T:"if" N:expr T:"then" N:stmt T:"else" N:stmt ),
            production!( G  stmt -> T:"if" N:expr T:"then" N:stmt ),
        ]})}

    fn ex_left_factor_2() -> ~StaticGrammar { let G = new_symbol_registry(); ~(G,Grammar {
        start: sym(G, core("stmt")),
        productions: ~[
            production!( G  S -> T:"i" N:E T:"t" N:S ),
            production!( G  S -> T:"i" N:E T:"t" N:S T:"e" N:S ),
            production!( G  S -> T:"a" ),
            production!( G  E -> T:"b" ),
        ]})}

    fn exercise_4_3_1_input() -> ~StaticGrammar {
        let G = new_symbol_registry(); ~(G,Grammar {
            start: sym(G, core("rexpr")),
            productions: ~[
                production!( G    rexpr -> N:rexpr T:"+" N:rterm ),
                production!( G    rexpr -> N:rterm               ),
                production!( G    rterm -> N:rterm N:rfactor     ),
                production!( G    rterm -> N:rfactor             ),
                production!( G  rfactor -> N:rfactor T:"*"       ),
                production!( G  rfactor -> N:rprimary            ),
                production!( G rprimary -> T:"a"                 ),
                production!( G rprimary -> T:"b"                 ),
        ]})
    }

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
            let bodies : ~[~[PS]] = prods.map(|p| { assert!(p.head == *a); (*p.body).clone() });
            let (alphas, betas) : (~[~[PS]], ~[~[PS]]) =
                bodies.partitioned(|b| b.len() > 0 && b[0].matches_nt(a));
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
            self.body.len() >= 1 && self.body[0].matches_nt(&self.head)
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

    struct LeftFactoring<T,NT> { orig: ~[Prod<T,NT>], fresh: ~[Prod<T,NT>], }

    fn left_factor_onestep<T:Eq+Ord+Clone, NT:Eq+Ord+Clone+Primable>(rules: &[Prod<T,NT>])
        -> Option<LeftFactoring<T,NT>> {
        use extra::sort;
        use std::vec;

        let rules = do sort::merge_sort(rules) |r1, r2| {
            *r1.body <= *r2.body
        };
        let i0 = rules.iter();
        let mut i1 = rules.iter(); i1.next();
        let (alt0, alt1) = match do i0.zip(i1).max_by() |&(alt0, alt1)| {
            alt0.body.maximal_common_prefix(&alt1.body).len()
        } {
            Some(e) => e,
            None => return None, // handle case where rules has only one element.
        };

        let a = alt0.head.clone();
        let alpha = alt0.body.maximal_common_prefix(&alt1.body);
        if alpha.len() == 0 {
            return None;
        }

        let a_prime = a.prime();
        let mut new_a_rules = ~[];
        let mut new_a2_rules = ~[];

        let new_a_body = vec::append_one(alpha.clone(), NT(a_prime.clone()));
        new_a_rules.push(production(a, new_a_body));

        for r in rules.iter() {
            match factor_suffix(alpha, *r.body) {
                None => new_a_rules.push(r.clone()),
                Some(suffix) => new_a2_rules.push(production(a_prime.clone(), suffix.to_owned()))
            }
        }

        Some(LeftFactoring{ orig: new_a_rules, fresh: new_a2_rules })
    }

    impl<T:Clone+TotalOrd,NT:Eq+Hash+Clone+TotalOrd+Primable> Grammar<T,NT> {

        fn to_nt_map(&self) -> TreeMap<NT, ~[Prod<T,NT>]> {
            type Rules = ~[Prod<T,NT>];
            type NTMap = TreeMap<NT, Rules>;
            let mut rules : NTMap = TreeMap::new();
            for p in self.productions.iter() {
                if match rules.find_mut(&p.head) {
                    Some(ref mut v) => { v.push(p.clone()); false },
                    None        => true,
                } {
                    rules.insert(p.head.clone(), ~[p.clone()]);
                }
            }
            rules
        }

        fn from_nt_map(start: &NT, rules: &TreeMap<NT, ~[Prod<T,NT>]>) -> Grammar<T,NT> {
            let mut new_prods : ~[Prod<T,NT>] = ~[];
            for (_nt, prods) in rules.iter() {
                new_prods.push_all(*prods);
            }
            Grammar { start: start.clone(), productions: new_prods }
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
        //       by the productions
        //         A_i -> \delta_1 \gamma | \delta_2 \gamma | ... | \delta_k \gamma,
        //       where A_j -> \delta_1 | \delta_2 | ... | \delta_k
        //       are all the current A_j-productions;
        //   end
        //   eliminate the immediate left recursion among the A_i productions
        // end

        fn eliminate_left_recursion(&self) -> Grammar<T,NT> {
            use std::vec;

            type Rules = ~[Prod<T,NT>];
            type Bodies = ~[PString<T,NT>];
            type NTMap = TreeMap<NT, Rules>;

            let mut rules : NTMap = self.to_nt_map();
            let keys : ~[NT] = rules.iter().map(|(k,_v)|k.clone()).collect();
            for i in range(0, keys.len()) {
                let A_i = keys[i].clone();
                for j in range(0, i) {
                    let A_j = keys[j].clone();

                    // replace each production of the form A_i -> A_j \gamma
                    //   by the productions { A_i -> \delta_l \gamma | l in {1..k} }
                    //   where A_j -> \delta_1 | \delta_2 | ... | \delta_k
                    //   are all the current A_j-productions;

                    let deltas : Bodies = rules.find(&A_j).unwrap().iter().map(|p|p.body.clone()).collect();
                    let mut new_rules : Rules = ~[];
                    for p in rules.find(&A_i).unwrap().iter() {
                        let body = p.body.clone();
                        if body.len() > 0 && body[0].matches_nt(&A_j) {
                            let gamma = body.slice_from(1);
                            for delta in deltas.iter().map(|pb|(**pb).clone()) {
                                new_rules.push(production(A_i.clone(), vec::append(delta, gamma)));
                            }
                        } else {
                            new_rules.push(p.clone());
                        }
                    }
                    rules.insert(A_i.clone(), new_rules);
                }

                let new_rules = eliminate_immediate_left_recursion(*rules.find(&A_i).unwrap());
                rules.insert(A_i, new_rules);
            }

            Grammar::from_nt_map(&self.start, &rules)
        }
    }

    impl<T:Clone+Eq+Ord+TotalOrd,NT:Clone+Eq+Ord+TotalOrd+Hash+Primable> Grammar<T,NT> {
        fn left_factor(&self) -> Grammar<T,NT> {
            use extra::sort;

            let mut rules = self.to_nt_map();
            loop {
                let keys : ~[NT] = rules.iter().map(|(k,_v)|k.clone()).collect();
                let mut changed = false;
                for k in keys.iter() {
                    match left_factor_onestep(*rules.find(k).unwrap()) {
                        None => {},
                        Some(LeftFactoring{ orig: orig, fresh: fresh }) => {
                            rules.insert(orig[0].head.clone(), orig);
                            rules.insert(fresh[0].head.clone(), fresh);
                            changed = true;
                            break;
                        }
                    }
                }
                if changed {
                    loop;
                } else {
                    break;
                }
            }
            Grammar::from_nt_map(&self.start, &rules)
        }
    }

    struct PredictiveParserGen<'self, T,NT> {
        grammar: &'self Grammar<T,NT>,
        precomputed_firsts: HashMap<NT, FirstSet<T>>,
        precomputed_follows: HashMap<NT, FollowSet<T>>,
    }

    type MidEntry<T,NT> = HashSet<Prod<T, NT>>;
    type EndEntry<T,NT> = HashSet<Prod<T, NT>>;

    struct MidTable<T,NT> {
        map: HashMap<(NT,T), MidEntry<T,NT>>
    }
    struct EndTable<T,NT> {
        map: HashMap<NT, EndEntry<T,NT>>
    }

    impl<T:Terminal,NT:NonTerminal> MidTable<T,NT> {
        fn new() -> MidTable<T,NT> { MidTable { map: HashMap::new() } }

        fn insert(&mut self, nt: NT, t: T, p: Prod<T,NT>) {
            fn new<T:Terminal, NT:NonTerminal>(_: &(NT,T), p: Prod<T,NT>)
                                               -> MidEntry<T,NT>
            {
                let mut s = HashSet::new();
                s.insert(p.clone());
                s
            }

            fn add<'a, T:Terminal,NT:NonTerminal>(_: &(NT,T),
                                                  prior: &'a mut MidEntry<T,NT>,
                                                  p: Prod<T,NT>)
            {
                prior.insert(p);
            }

            self.map.mangle((nt, t), p, new::<T,NT>, add::<T,NT>);
        }
        fn entries(&self, _nt: NT, _input: T, _visit: &fn (&Prod<T,NT>)) {
            fail!("MidTable entries unimplemented");
        }
    }

    impl <T,NT:IterBytes+Eq> EndTable<T,NT> {
        fn new() -> EndTable<T,NT> { EndTable { map: HashMap::new() } }

        fn insert(&mut self, nt: NT, prod: Prod<T,NT>) {
            self.map.mangle(nt,
                            &prod, 
                            new_end_entry::<T,NT>,
                            mod_end_entry::<T,NT>);
        }
        fn entries(&self, _nt: NT, _visit: &fn (&Prod<T,NT>)) {
            fail!("EndTable entries unimplemented");
        }
    }

    struct PredictiveParsingTable<T,NT> {
        terms: HashSet<T>,
        nonterms: HashSet<NT>,
        mid: MidTable<T,NT>,
        end: EndTable<T,NT>,
    }

    enum Cell<'self, T,NT> {
        MidCell(&'self T, &'self NT, &'self Prod<T,NT>),
        EndCell(&'self NT, &'self Prod<T,NT>),
    }

    impl<T:Terminal,NT:NonTerminal> PredictiveParsingTable<T,NT> {
        fn each_term(&self, visit: &fn(&T)) {
            for t in self.terms.iter() { visit(t); }
        }
        fn each_nonterm(&self, visit: &fn(&NT)) {
            for nt in self.nonterms.iter() { visit(nt); }
        }
        fn each_prod(&self, key: &(NT, T), visit: &fn(&Prod<T,NT>)) {
            match self.mid.map.find(key) {
                None => {},
                Some(s) => { for p in s.iter() { visit(p); } }
            }
        }
        fn each_end_prod(&self, key: &NT, visit: &fn(&Prod<T,NT>)) {
            match self.end.map.find(key) {
                None => {},
                Some(s) => { for p in s.iter() { visit(p); } }
            }
        }

        fn each_cell(&self, visit: &fn(&Cell<T,NT>)) {
            do self.each_term |t| {
                do self.each_nonterm |nt| {
                    let key = (nt.clone(), t.clone());
                    do self.each_prod(&key) |p| {
                        let c = MidCell(t, nt, p);
                        visit(&c);
                    }
                }
            }
            do self.each_nonterm |nt| {
                do self.each_end_prod(nt) |p| {
                    let c = EndCell(nt, p);
                    visit(&c);
                }
            }
        }
    }

    // A parsing table has its rows labelled by non-terminals and its
    // columns labelled by terminals.

    impl<T:Terminal+ToStr,NT:NonTerminal+ToStr> ToStr
        for PredictiveParsingTable<T,NT> {

        fn to_str(&self) -> ~str {

            // For proper formatting, we need to know the maximum
            // width of all productions in a column (as well as the
            // length of the string for the token itself labelling the
            // column).

            let mut width_map : ~[int] = ~[];

            // Copy terms and nonterms into vectors to keep stable order
            let terms : ~[T] = self.terms.iter().map(|x|x.clone()).collect();
            let nonterms : ~[NT] = self.nonterms.iter().map(|x|x.clone()).collect();

            for t in terms.iter() {
                let mut max_width = t.to_str().len() as int;
                for nt in nonterms.iter() {
                    let key = (nt.clone(), t.clone());
                    do self.each_prod(&key) |p| {
                        let l = p.to_str().len() as int;
                        if l > max_width { max_width = l }
                    }
                }
                width_map.push(max_width);
            }

            let mut row_header_width = 0;
            for nt in nonterms.iter() {
                let l = nt.to_str().len();
                if l > row_header_width { row_header_width = l; }
            }

            let mut s = " ".repeat(row_header_width) + " |";
            let mut row_width = 0;
            for i in range(0, terms.len()) {
                let ref t = terms[i];
                let len = width_map[i];
                let t = t.to_str();
                let remainder = len - (t.len() as int);
                let left = remainder / 2;
                let right = remainder - left;
                if left < 0 || right < 0 { fail!("negative pad value(s)."); }

                let left = " ".repeat(left as uint);
                let right = " ".repeat(right as uint);
                s = s + "| " + left + "`" + t + "`" + right  + " ";
                row_width = s.len();
            }
            s = s + "\n";
            s = s + "=".repeat(row_width) + "\n";

            for nt in nonterms.iter() {
                let mut entries : ~[~[Prod<T,NT>]] = ~[];
                let mut max_len = 0;
                for t in terms.iter() {
                    let mut prods : ~[Prod<T,NT>] = ~[];
                    let key = (nt.clone(), t.clone());
                    do self.each_prod(&key) |p| { prods.push(p.clone()); }
                    if prods.len() > max_len {
                        max_len = entries.len();
                    }
                    entries.push(prods);
                }

                let nt = nt.to_str();
                assert!(nt.len() <= row_header_width);
                { let remainder = row_header_width - nt.len();
                  let left = " ".repeat(remainder);
                  s = s + left + nt + " |";
                }

                for i in range(0, max_len) {
                    for j in range(0, terms.len()) {
                        let len = width_map[j];

                        if i < entries[j].len() {
                            let p = entries[j][i].to_str();
                            let remainder = len - (p.len() as int);
                            let left = remainder / 2;
                            let right = remainder - left;
                            if left < 0 || right < 0 {
                                println!("width_map: {:?}", width_map);
                                println!("prod: {}", p);
                                println!("remainder: {} left: {} right: {}",
                                         remainder, left, right);
                            }
                            assert!(left >= 0);
                            assert!(right >= 0);
                            if left < 0 || right < 0 { fail!("negative pad value(s)."); }
                            let left = " ".repeat(left as uint);
                            let right = " ".repeat(right as uint);
                            s = s + "| " + left + " " + p + " " + right + " ";
                        } else {
                            assert!(len >= 0);
                            let fill = " ".repeat(len as uint);
                            s = s + "|  " + fill + "  ";
                        }
                    }
                    s = s + "\n";
                }
            }

            s
        }
    }

    enum FirstSet<T> {
        Empty,
        Term(T),
        Many{beginnings: HashSet<T>, has_epsilon: bool}
    }

    impl<T:ToStr+Eq+Hash> ToStr for FirstSet<T> {
        fn to_str(&self) -> ~str {
            match self {
                &Empty   => ~"{}",
                &Term(ref t) => ~"{" + t.to_str() + "}",
                &Many{beginnings: ref b, has_epsilon: e} => {
                    let mut seen = false;
                    let ret = do b.iter().fold(~"{") |b, a| {
                        let ret = if seen {
                            b+", "+a.to_str()
                        } else {
                            b+a.to_str()
                        };
                        seen = true;
                        ret
                    };
                    let ret = if e {
                        let epsilon = "\u03B5";
                        (if seen { ret + ", " } else { ret }) + epsilon
                    } else {
                        ret
                    };
                    ret + "}"
                }
            }
        }
    }

    impl<T:Eq+IterBytes+Clone> FirstSet<T> {
        fn contains_epsilon(&self) -> bool {
            match self {
                &Empty => false,
                &Term(*) => false,
                &Many{ beginnings: _, has_epsilon: b } => b,
            }
        }

        fn termless_many(has_epsilon: bool) -> ~FirstSet<T> {
            ~Many{ beginnings: HashSet::new(), has_epsilon: has_epsilon }
        }

        fn singleton_many(t: T, has_epsilon: bool) -> ~FirstSet<T> {
            let mut s = HashSet::new();
            s.insert(t);
            ~Many{ beginnings: s, has_epsilon: has_epsilon }
        }

        fn add_epsilon(~self) -> ~FirstSet<T> {
            match self {
                ~Empty   => FirstSet::termless_many(true),
                ~Term(t) => FirstSet::singleton_many(t, true),
                ~Many{ beginnings: b, has_epsilon: _ } => {
                    ~Many{ beginnings: b, has_epsilon: true }
                }
            }
        }

        fn union(~self, other: &FirstSet<T>) -> ~FirstSet<T> {
            let mut recv = match self {
                ~Empty   => FirstSet::termless_many(false),
                ~Term(t) => FirstSet::singleton_many(t, false),
                ~Many{ beginnings: _, has_epsilon: _ } => self
            };
            match recv {
                ~Empty | ~Term(*) => fail!("cannot happen now"),
                ~Many{ beginnings: ref mut b, has_epsilon: ref mut e } => {
                    match other {
                        &Empty => {},
                        &Term(ref t) => { b.insert(t.clone()); },
                        &Many{ beginnings: ref c, has_epsilon: ref f } => {
                            for s in c.iter() {
                                b.insert(s.clone());
                            }
                            *e |= *f;
                        }
                    }
                }
            }
            recv
        }
    }

    impl<T:IterBytes+Eq> FirstSet<T> {
        fn for_each_term(&self, f: &fn (&T) -> ()) {
            match self {
                &Empty => {},
                &Term(ref t) => f(t),
                &Many{ beginnings: ref b, has_epsilon: _ } => {
                    b.iter().all(|x| { f(x); true });
                },
            }
        }
        fn has_epsilon(&self) -> bool {
            match self {
                &Empty | &Term(*) => false,
                &Many{ beginnings: _, has_epsilon: he } => he,
            }
        }
    }

    struct FollowSet<T> {
        right_neighbors: HashSet<T>,
        can_terminate: bool,
    }

    impl<T:ToStr+IterBytes+Eq> ToStr for FollowSet<T> {
        fn to_str(&self) -> ~str {
            let mut seen = false;
            let ret = do self.right_neighbors.iter().fold(~"{") |b, a| {
                if seen {
                    b+", "+a.to_str()
                } else {
                    seen = true;
                    b+a.to_str()
                }
            };
            let ret = if self.can_terminate {
                if seen { ret + ", $" } else { ret + "$" }
            } else {
                ret
            };
            ret + "}"
        }
    }

    impl<T:Clone+Eq+IterBytes> FollowSet<T> {
        fn just_end_marker() -> FollowSet<T> {
            FollowSet{ right_neighbors: HashSet::new(), can_terminate: true }
        }
    }

    fn new_end_entry<T,NT>(_nt: &NT, _prod: &Prod<T,NT>) -> EndEntry<T,NT> {
        fail!("unimplemented");
    }

    fn mod_end_entry<'a,T,NT>(_nt: &NT,
                              _prior: &'a mut EndEntry<T,NT>,
                              _prod: &Prod<T,NT>) {
        fail!("unimplemented");
    }


    impl<'self, T:Terminal, NT:NonTerminal>
        PredictiveParserGen<'self, T,NT>
    {
        fn make_parsing_table(&self) -> PredictiveParsingTable<T,NT> {
            type Rule = Prod<T,NT>;
            let mut mid_table = MidTable::new();
            let mut end_table = EndTable::new();
            let mut terms     = HashSet::new();
            let mut nonterms  = HashSet::new();

            for p in self.grammar.productions_iter() {
                let A : NT = p.head.clone();
                let newA = || A.clone();
                nonterms.insert(newA());
                let alpha : [ProductionSym<T,NT>, ..1] = [NT(newA())];
                let first = self.first(alpha);
                do first.for_each_term |a| {
                    terms.insert(a.clone());
                    mid_table.insert(newA(), a.clone(), p.clone());
                }
                if first.has_epsilon() {
                    let follow = self.follow(A);
                    for b in follow.right_neighbors.iter() {
                        terms.insert(b.clone());
                        mid_table.insert(newA(), b.clone(), p.clone());
                    }
                    if follow.can_terminate {
                        end_table.insert(newA(), p.clone());
                    }
                }
            }

            PredictiveParsingTable {
                terms:    terms,
                nonterms: nonterms,
                mid:      mid_table,
                end:      end_table,
            }
        }

        fn make<'a>(grammar: &'a Grammar<T,NT>) -> PredictiveParserGen<'a, T,NT> {
            let mut first : HashMap<NT, FirstSet<T>> = HashMap::new();

            for p in grammar.productions_iter() {
                if p.body.len() == 0 {
                    first.insert(p.head.clone(),
                                 Many{ beginnings: HashSet::new(),
                                       has_epsilon: true });
                }
            }

            loop {
                let mut any_changed = false;

                for p in grammar.productions_iter() {
                    let mut to_add : HashSet<T> = HashSet::new();
                    let mut all_had_epsilon = true;
                    let update = |first_set:&FirstSet<T>| -> bool {
                        do first_set.for_each_term |s| {
                            to_add.insert(s.clone());
                        }
                        if !first_set.has_epsilon() {
                            all_had_epsilon = false;
                            true
                        } else {
                            false
                        }
                    };
                    for s in p.body.iter() {
                        match s {
                            &T(ref t) => {
                                let first_set = Term(t.clone());
                                if update(&first_set) {
                                    break;
                                }
                            },

                            &NT(ref nt) => {
                                match first.find(nt) {
                                    None => {
                                        all_had_epsilon = false;
                                        break;
                                    }, // wait until entry is filled later.
                                    Some(first_set) => {
                                        if update(first_set) {
                                            break;
                                        }
                                    }
                                }
                            }
                        };
                    }

                    let fresh_entry = |_:&NT, to_add| {
                        any_changed = true;
                        Many{ beginnings: to_add, has_epsilon: all_had_epsilon, }
                    };
                    let update_entry = |_:&NT, prior: &mut FirstSet<T>, to_add:HashSet<T>| {
                        let action = match prior {
                            &Empty => {
                                Some(Many{ beginnings: to_add, has_epsilon: all_had_epsilon })
                            }
                            &Term(ref t) => {
                                let mut to_add = to_add.clone();
                                to_add.insert(t.clone());
                                Some(Many{ beginnings: to_add.clone(), has_epsilon: all_had_epsilon })
                            },
                            &Many{beginnings: ref mut begin_recv, has_epsilon: ref mut eps_recv} => {
                                for t in to_add.iter() {
                                    if begin_recv.insert(t.clone()) {
                                        any_changed = true;
                                    }
                                }
                                if !*eps_recv && all_had_epsilon {
                                    *eps_recv = true;
                                    any_changed = true;
                                }
                                None
                            }
                        };
                        match action {
                            Some(p) => *prior = p,
                            None => {}
                        }
                    };
                    first.mangle(p.head.clone(), to_add, fresh_entry, update_entry);
                }

                if !any_changed {
                    break;
                }
            }

            let prefollows = PredictiveParserGen {
                grammar: grammar,
                precomputed_firsts: first,
                precomputed_follows: HashMap::new(),
            };

            let mut follows : HashMap<NT, FollowSet<T>> = HashMap::new();
            follows.insert(grammar.start.clone(), FollowSet::just_end_marker());
            loop {
                let mut any_change = false;

                // Production A -> α B β
                // implies FOLLOW(B) := FOLLOW(B) U (FIRST(β) \ {ε})
                //
                // Production A -> α B or A -> α B β where ε in FIRST(β)
                // imples FOLLOW(B) := FOLLOW(B) U FOLLOW(A)

                for p in grammar.productions_iter() {

                    for i in range(0, p.body.len()) {

                        let fresh_from_first = |_:&NT, first_beta:&FirstSet<T>| {
                            any_change = true;
                            let mut s = HashSet::new();
                            do first_beta.for_each_term |t| {
                                s.insert(t.clone());
                            }
                            FollowSet{ right_neighbors: s,
                                       can_terminate: false }
                        };
                        let update_from_first =
                            |_:&NT, prior: &mut FollowSet<T>, first_beta:&FirstSet<T>| {
                            do first_beta.for_each_term |t| {
                                if prior.right_neighbors.insert(t.clone()) {
                                    any_change = true;
                                }
                            }
                        };

                        let fresh_from_follow = |_:&NT, follow_A:&FollowSet<T>| {
                            any_change = true;
                            FollowSet {
                                right_neighbors: follow_A.right_neighbors.clone(),
                                can_terminate: follow_A.can_terminate
                            }
                        };

                        let update_from_follow =
                            |_:&NT, prior: &mut FollowSet<T>, follow_A:&FollowSet<T>| {
                            for r in follow_A.right_neighbors.iter() {
                                if prior.right_neighbors.insert(r.clone()) {
                                    any_change = true;
                                }
                            }
                            if !prior.can_terminate && follow_A.can_terminate {
                                any_change = true;
                                prior.can_terminate = true;
                            }
                        };

                        match p.body[i] {
                            T(*) => {},
                            NT(ref B) => {
                                let beta = p.body.slice(i+1, p.body.len());
                                let first_beta = prefollows.first(beta);

                                let act =
                                    if first_beta.contains_epsilon() {
                                    let A = p.head.clone();
                                    match follows.find(&A) {
                                        None => None, // wait until we find it later
                                        Some(ref f) => Some(FollowSet{
                                                right_neighbors: f.right_neighbors.clone(),
                                                can_terminate: f.can_terminate,
                                            })
                                    }
                                } else {
                                    None
                                };

                                follows.mangle(B.clone(),
                                               &first_beta,
                                               fresh_from_first,
                                               update_from_first);

                                match act {
                                    None => {},
                                    Some(ref f) => {
                                        follows.mangle(B.clone(),
                                                       f,
                                                       fresh_from_follow,
                                                       update_from_follow);
                                    }
                                }
                            }
                        }
                    }
                }

                if !any_change {
                    break;
                }
            }

            PredictiveParserGen{
                grammar: grammar,
                precomputed_firsts: prefollows.precomputed_firsts,
                precomputed_follows: follows,
            }
        }
        fn first_for_term(&self, t: T) -> FirstSet<T> {
            Term(t)
        }
        fn first_for_nonterm<'a>(&'a self, nt: &NT) -> &'a FirstSet<T> {
            self.precomputed_firsts.get(nt)
        }

        fn first(&self, alpha: &[ProductionSym<T,NT>]) -> FirstSet<T> {
            let mut accum = ~Empty;
            let mut all_contain_epsilon = true;
            for s in alpha.iter() {
                match s {
                    &T(ref t) => {
                        accum = accum.union(&self.first_for_term(t.clone()));
                        all_contain_epsilon = false;
                        break;
                    },
                    &NT(ref nt) => {
                        let f = self.first_for_nonterm(nt);
                        accum = accum.union(f);
                        if !f.contains_epsilon() {
                            all_contain_epsilon = false;
                            break;
                        }
                    }
                }
            }
            // N.B. If alpha is empty, we get right answer here (inherently).
            if all_contain_epsilon {
                accum = accum.add_epsilon();
            }
            *accum
        }
        fn follow<'a>(&'a self, A: NT) -> &'a FollowSet<T> {
            self.precomputed_follows.get(&A)
        }
    }

    #[test]
    fn elim_immed_left_rec() {
        let g = eliminate_immediate_left_recursion(ex_elim_amb_1().owned_productions());
        println(fmt!("%s\n", g.to_str()));
    }

    #[test]
    fn elim_left_rec() {
        let g = ex_left_recur_1().owned_grammar().eliminate_left_recursion();
        println(fmt!("%s\n", g.to_str()));
    }

    #[test]
    fn left_factor() {
        println(fmt!("left_factor_1:\n%s\n",
                     ex_left_factor_1().to_str()));
        println(fmt!("left_factor_1.left_factor():\n%s\n",
                     ex_left_factor_1().owned_grammar().left_factor().to_str()));
        println(fmt!("left_factor_2:\n%s\n",
                     ex_left_factor_2().to_str()));
        println(fmt!("left_factor_2.left_factor():\n%s\n",
                     ex_left_factor_2().owned_grammar().left_factor().to_str()));
    }

    #[test]
    fn exercise_4_3_1() {
        let g = exercise_4_3_1_input();
        println(fmt!("4_3_1:\n%s\n", g.to_str()));
        let h = g.to_grammar().left_factor();
        println(fmt!("4_3_1 left factored:\n%s\n", h.to_str()));
        let i = h.eliminate_left_recursion();
        println(fmt!("4_3_1 left factored, left rec elim:\n%s\n", i.to_str()));
    }

    #[test]
    fn whoa() {
        let ex4_5 = example_4_5();
        println(fmt!("%s\n", ex4_5.to_str()));
        println(fmt!("%s\n", example_4_6().to_str()));
        println(fmt!("%s\n", example_4_7().to_str()));
        println(fmt!("%s\n", example_4_13().to_str()));
        println(fmt!("%s\n", ex_elim_amb_1().to_str()));
        println(fmt!("left_recur_1:\n%s\n", ex_left_recur_1().to_str()));
        println(fmt!("left_recur_2:\n%s\n", ex_left_recur_2().to_str()));
    }

    #[test]
    fn first() {
        fn go(name: ~str,
              maker: &fn() -> ~StaticGrammar,
              make_alpha: &fn(&SymbolRegistry) -> StaticStr) {
            let ~(syms, ref g) = maker();
            let ppg = PredictiveParserGen::make(g);
            let alpha : StaticStr = make_alpha(&syms);
            println(fmt!("%s alpha: %s", name, alpha.to_str()));
            println(fmt!("FIRST(alpha): %s", ppg.first(*alpha).to_str()));
        }

        do go(~"eg 4.5", example_4_5) |syms| {
            PString(~[ NT(syms.sym("expression")),
                       ]) }

        do go(~"eg 4.13", example_4_13) |syms| {
            PString(~[ NT(syms.sym("S")),
                       NT(syms.sym("S")),
                       T("h"),
                       ]) }

        do go(~"ex 4.2.1", exercise_4_2_1) |syms| {
            PString(~[ NT(syms.sym("S")),
                       NT(syms.sym("S")),
                       ]) }

        do go(~"ex 4.2.1", ex_elim_amb_1) |syms| {
            PString(~[ NT(syms.sym("stmt")),
                       NT(syms.sym("stmt")),
                       ]) }

        do go(~"ex elim amb 2", ex_elim_amb_2) |syms| {
            PString(~[ NT(syms.sym("stmt")),
                       NT(syms.sym("unmatched_stmt")),
                       ]) }

        do go(~"ex elim amb 2", ex_elim_amb_2) |syms| {
            PString(~[ NT(syms.sym("unmatched_stmt")),
                       NT(syms.sym("stmt")),
                       ]) }

        do go(~"extra (empty) case 4.5", example_4_5) |_syms| {
            PString(~[ ]) }
    }

    #[test]
    fn follow() {
        let ~(syms, ref g) = example_4_5();
        let ppg = PredictiveParserGen::make(g);
        let t = syms.sym("expression");
        // notably, "id" is not in FOLLOW(<expression>)
        println(fmt!("ex 4.5 T: %s FOLLOW(T): %s",
                     t.to_str(),
                     ppg.follow(t).to_str()));

        let ~(syms, ref g) = ex_elim_amb_2();
        let ppg = PredictiveParserGen::make(g);
        let t = syms.sym("expr");
        // notably, "id" is not in FOLLOW(<expression>)
        println(fmt!("ex elim amb 2 T: %s FOLLOW(T): %s",
                     t.to_str(),
                     ppg.follow(t).to_str())); 
        let t = syms.sym("matched_stmt");
        println(fmt!("ex elim amb 2 T: %s FOLLOW(T): %s",
                     t.to_str(),
                     ppg.follow(t).to_str()));
        // FOLLOW(<unmatched_stmtm>) ?= { $ } ?
        let t = syms.sym("unmatched_stmt");
        println(fmt!("ex elim amb 2 T: %s FOLLOW(T): %s",
                     t.to_str(),
                     ppg.follow(t).to_str()));
    }

    #[test]
    fn test_make_table() {
        let ~(_syms, ref g) = example_4_5();
        let ppg = PredictiveParserGen::make(g);
        let table = ppg.make_parsing_table();
        println(fmt!("grammar: %s, parsing_table: \n%s", g.to_str(), table.to_str()));
    }

    fn iter_to_vec<'a, X:Clone, I:Iterator<X>>(i:I) -> ~[X] {
        i.map(|x| x.clone()).collect()
    }
    fn set_to_vec<X:Eq+Hash+Clone>(s:&HashSet<X>) -> ~[X] {
        s.iter().map(|x| x.clone()).collect()
    }
}
