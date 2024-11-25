use core::panic;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type NonTerminal = String;
type Terminal = String;
type Symbol = String;

pub struct CFG {
    terminals: HashSet<Rc<Terminal>>,
    non_terminals: HashSet<Rc<NonTerminal>>,
    productions: HashMap<Rc<NonTerminal>, HashSet<Vec<Rc<Symbol>>>>,
    start_symbol: Rc<Terminal>,
}

impl CFG {
    pub fn new<I, S, K>(terminals: I, non_terminals: I, productions: K, start_symbol: S) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
        K: IntoIterator<Item = (S, Vec<S>)>,
    {
        let terminals = terminals
            .into_iter()
            .map(|s| s.into())
            .collect::<HashSet<_>>();

        let non_terminals = non_terminals
            .into_iter()
            .map(|s| s.into())
            .collect::<HashSet<_>>();

        let productions = productions
            .into_iter()
            .map(|(lhs, rhs)| {
                (
                    lhs.into(),
                    rhs.into_iter()
                        .map(|symbol| symbol.into())
                        .collect::<Vec<String>>(),
                )
            })
            .collect::<HashSet<_>>();

        let start_symbol = start_symbol.into();

        // non_terminals と terminals に重複があったら panic
        for symbol in terminals.iter() {
            if non_terminals.contains(symbol) {
                panic!("'{}' is included in both terminals and non_terminals. They must be mutually disjoint.", symbol);
            }
        }

        // start_symbol と productions の validation は一旦後にして、データ整形
        let mut t_s = HashSet::new();
        for a in terminals {
            t_s.insert(Rc::new(a));
        }

        let mut nt_s = HashSet::new();
        let mut p_s = HashMap::new();
        for x in non_terminals {
            let tmp = Rc::new(x);
            let tmp2 = tmp.clone();
            nt_s.insert(tmp);
            p_s.insert(tmp2, HashSet::new());
        }

        // productions の validation とデータ整形をまとめてに行う
        for (p_lhs, p_rhs) in productions {
            // productions の左辺が non_terminals の元になっているか？
            if let Some(lhs) = nt_s.get(&p_lhs) {
                // productions の右辺のVec の要素が non_terminals もしくは terminals の元になっているか？
                let mut rhs = vec![];
                for p_symbol in p_rhs {
                    if let Some(rhs_element) = nt_s.get(&p_symbol) {
                        rhs.push(rhs_element.clone());
                        continue;
                    }

                    if let Some(rhs_element) = t_s.get(&p_symbol) {
                        rhs.push(rhs_element.clone());
                        continue;
                    }

                    panic!(
                        "'{}' is neither included in non_terminals nor terminals.",
                        p_symbol
                    );
                }
                p_s.get_mut(lhs).unwrap().insert(rhs);
            } else {
                panic!("'{}' is not included in non_terminals.", p_lhs);
            }
        }

        // start_symbol の validation とデータ整形をまとめて行う
        // start_symbol が non_terminal の元になっているか？
        let s;
        if let Some(ss) = nt_s.get(&start_symbol) {
            s = ss.clone();
        } else {
            panic!(
                "'{}' is not included in non_terminals. It must be included in non_terminals.",
                start_symbol
            );
        }

        CFG {
            terminals: t_s,
            non_terminals: nt_s,
            productions: p_s,
            start_symbol: s,
        }
    }

    fn calculate_nullables(&self) -> HashMap<Rc<NonTerminal>, bool> {
        let mut nullables = self
            .non_terminals
            .iter()
            .map(|key| (key.clone(), false))
            .collect::<HashMap<Rc<NonTerminal>, bool>>();

        let mut nullables_current = nullables.clone();

        // 最小不動点を計算する
        loop {
            let mut changed = false;
            for x in self.non_terminals.iter() {
                let current = *nullables_current.get(x).unwrap();

                let rhs_s = self.productions.get(x).unwrap();
                for rhs in rhs_s.iter() {
                    let new = rhs.iter().all(|symbol| {
                        if self.terminals.contains(symbol) {
                            false
                        } else {
                            *nullables_current.get(symbol).unwrap()
                        }
                    });
                    if current != new {
                        assert_eq!(new, true); // new は true のはず
                        *nullables.get_mut(x).unwrap() = new;
                        changed = true;
                    }
                }
            }

            nullables_current = nullables.clone();

            if changed {
                break;
            }
        }

        nullables
    }
}

#[cfg(test)]
mod cfg_tests {
    use super::*;

    #[test]
    fn create_valid_grammar() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "E'", "T", "T'", "F"];

        let productions = vec![
            ("E", vec!["T", "E'"]),
            ("E'", vec!["+", "T", "E'"]),
            ("E'", vec![]),
            ("T", vec!["F", "T'"]),
            ("T'", vec!["*", "F", "T'"]),
            ("T'", vec![]),
            ("F", vec!["(", "E", ")"]),
            ("F", vec!["i"]),
        ];

        let start_symbol = "E";

        let _ = CFG::new(terminals, non_terminals, productions, start_symbol);
    }

    #[test]
    #[should_panic(
        expected = "'i' is included in both terminals and non_terminals. They must be mutually disjoint."
    )]
    fn pass_non_disjoint_terminals_and_nonterminals() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "E'", "T", "T'", "F", "i"];

        let productions = vec![
            ("E", vec!["T", "E'"]),
            ("E'", vec!["+", "T", "E'"]),
            ("E'", vec![]),
            ("T", vec!["F", "T'"]),
            ("T'", vec!["*", "F", "T'"]),
            ("T'", vec![]),
            ("F", vec!["(", "E", ")"]),
            ("F", vec!["i"]),
        ];

        let start_symbol = "E";

        let _ = CFG::new(terminals, non_terminals, productions, start_symbol);
    }

    #[test]
    #[should_panic(expected = "'?' is neither included in non_terminals nor terminals.")]
    fn pass_productions_with_symbol_included_neither_non_terminals_nor_terminals_in_rhs() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "E'", "T", "T'", "F"];

        let productions = vec![
            ("E", vec!["T", "E'"]),
            ("E'", vec!["+", "T", "E'"]),
            ("E'", vec!["?"]),
            ("T", vec!["F", "T'"]),
            ("T'", vec!["*", "F", "T'"]),
            ("T'", vec![]),
            ("F", vec!["(", "E", ")"]),
            ("F", vec!["i"]),
        ];

        let start_symbol = "E";

        let _ = CFG::new(terminals, non_terminals, productions, start_symbol);
    }

    #[test]
    #[should_panic(expected = "'T' is not included in non_terminals.")]
    fn pass_productions_with_lhs_not_included_in_non_terminals() {
        let terminals = vec!["+", "*", "i", "(", ")", "T"];

        let non_terminals = vec!["E", "E'", "T'", "F"];

        let productions = vec![
            ("E", vec!["T", "E'"]),
            ("E'", vec!["+", "T", "E'"]),
            ("E'", vec![]),
            ("T", vec!["F", "T'"]),
            ("T'", vec!["*", "F", "T'"]),
            ("T'", vec![]),
            ("F", vec!["(", "E", ")"]),
            ("F", vec!["i"]),
        ];

        let start_symbol = "E";

        let _ = CFG::new(terminals, non_terminals, productions, start_symbol);
    }

    #[test]
    #[should_panic(
        expected = "'G' is not included in non_terminals. It must be included in non_terminals."
    )]
    fn pass_start_symbol_not_included_in_non_terminals() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "E'", "T", "T'", "F"];

        let productions = vec![
            ("E", vec!["T", "E'"]),
            ("E'", vec!["+", "T", "E'"]),
            ("E'", vec![]),
            ("T", vec!["F", "T'"]),
            ("T'", vec!["*", "F", "T'"]),
            ("T'", vec![]),
            ("F", vec!["(", "E", ")"]),
            ("F", vec!["i"]),
        ];

        let start_symbol = "G";

        let _ = CFG::new(terminals, non_terminals, productions, start_symbol);
    }

    #[test]
    fn calculate_nullables() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "E'", "T", "T'", "F"];

        let productions = vec![
            ("E", vec!["T", "E'"]),
            ("E'", vec!["+", "T", "E'"]),
            ("E'", vec![]),
            ("T", vec!["F", "T'"]),
            ("T'", vec!["*", "F", "T'"]),
            ("T'", vec![]),
            ("F", vec!["(", "E", ")"]),
            ("F", vec!["i"]),
        ];

        let start_symbol = "E";

        let g2 = CFG::new(terminals, non_terminals, productions, start_symbol);

        let nullables = g2.calculate_nullables();

        let nullables_true = nullables
            .into_iter()
            .filter(|(_, t)| *t)
            .map(|(x, _)| x)
            .collect::<HashSet<_>>();

        let mut nullables_true_expected = HashSet::new();
        nullables_true_expected.insert(Rc::new("E'".to_string()));
        nullables_true_expected.insert(Rc::new("T'".to_string()));

        assert_eq!(nullables_true, nullables_true_expected);
    }

}
