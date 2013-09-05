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
    // use std::hashmap::HashMap;
    use extra::treemap::TreeMap;
    use std::to_bytes;


    trait Terminal { }
    trait NonTerminal { }

    #[deriving(Clone,Eq)]
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

    #[deriving(Clone)]
    struct PBody<T, NT>(~[ProductionSym<T, NT>]);

    impl<T:Eq+Clone,NT:Eq+Clone> PBody<T,NT> {
        fn maximal_common_prefix(&self, other: &PBody<T,NT>) -> ~[ProductionSym<T,NT>] {
            maximal_common_prefix(**self, **other)
        }
    }

    #[deriving(Clone)]
    struct Prod<T, NT> { head: NT, body: PBody<T, NT> }

    struct Grammar<T, NT> {
        start: NT,
        productions: ~[Prod<T, NT>],
    }

    trait ToGrammar<T, NT> {
        fn to_grammar(&self) -> Grammar<T, NT>;
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
            self.productions.map(|x|x.to_str_head_aligned(w)).connect("\n")
        }
    }

    fn production<T,NT>(h:NT, b: ~[ProductionSym<T,NT>]) -> Prod<T,NT> {
        Prod { head:h, body:PBody(b) }
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

    fn ex_left_factor_1() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("stmt")),
        productions: ~[
            production!( G  stmt -> T:"if" N:expr T:"then" N:stmt T:"else" N:stmt ),
            production!( G  stmt -> T:"if" N:expr T:"then" N:stmt ),
        ]}}

    fn ex_left_factor_2() -> StaticGrammar { let G = new_symbol_registry(); Grammar {
        start: sym(G, core("stmt")),
        productions: ~[
            production!( G  S -> T:"i" N:E T:"t" N:S ),
            production!( G  S -> T:"i" N:E T:"t" N:S T:"e" N:S ),
            production!( G  S -> T:"a" ),
            production!( G  E -> T:"b" ),
        ]}}

    fn exercise_4_3_1_input() -> StaticGrammar {
        let G = new_symbol_registry(); Grammar {
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
        ]}
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

        fn eliminate_left_recursion(&self) -> Grammar<T,NT> {
            use std::vec;

            type Rules = ~[Prod<T,NT>];
            type Bodies = ~[PBody<T,NT>];
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
    fn elim_immed_left_rec() {
        let g = eliminate_immediate_left_recursion(ex_elim_amb_1().productions);
        println(fmt!("%s\n", g.to_str()));
    }

    #[test]
    fn elim_left_rec() {
        let g = ex_left_recur_1().eliminate_left_recursion();
        println(fmt!("%s\n", g.to_str()));
    }

    #[test]
    fn left_factor() {
        println(fmt!("left_factor_1:\n%s\n",
                     ex_left_factor_1().to_str()));
        println(fmt!("left_factor_1.left_factor():\n%s\n",
                     ex_left_factor_1().left_factor().to_str()));
        println(fmt!("left_factor_2:\n%s\n",
                     ex_left_factor_2().to_str()));
        println(fmt!("left_factor_2.left_factor():\n%s\n",
                     ex_left_factor_2().left_factor().to_str()));
    }

    #[test]
    fn exercise_4_3_1() {
        let g = exercise_4_3_1_input();
        println(fmt!("4_3_1:\n%s\n", g.to_str()));
        let h = g.left_factor();
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
}

fn main() {
    println("Hello World");
}
