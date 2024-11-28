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

    pub fn calculate_nullables(&self) -> HashMap<Rc<NonTerminal>, bool> {
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
                let mut new = false;
                for rhs in rhs_s.iter() {
                    new = new
                        || rhs.iter().all(|symbol| {
                            if self.terminals.contains(symbol) {
                                false
                            } else {
                                *nullables_current.get(symbol).unwrap()
                            }
                        });
                }
                if current != new {
                    debug_assert_eq!(new, true); // new は true のはず
                    *nullables.get_mut(x).unwrap() = new;
                    changed = true;
                }
            }

            nullables_current = nullables.clone();

            if !changed {
                break;
            }
        }

        nullables
    }

    pub fn calculate_first_sets(&self) -> HashMap<Rc<NonTerminal>, HashSet<Rc<Terminal>>> {
        let mut first_sets = self
            .non_terminals
            .iter()
            .map(|x| (x.clone(), HashSet::new()))
            .collect::<HashMap<Rc<NonTerminal>, HashSet<Rc<Terminal>>>>();

        let mut first_sets_current = first_sets.clone();

        let nullables = self.calculate_nullables();

        // 最小不動点の計算
        loop {
            let mut changed = false;

            for x in self.non_terminals.iter() {
                let rhs_s = self.productions.get(x).unwrap();

                let mut new = HashSet::new();

                for rhs in rhs_s.iter() {
                    for symbol in rhs.iter() {
                        if self.terminals.contains(symbol) {
                            new.insert(symbol.clone());
                            break;
                        } else {
                            new.extend(first_sets_current.get(symbol).unwrap().iter().cloned());
                            if !nullables.get(symbol).unwrap() {
                                break;
                            }
                        }
                    }
                }

                let current = first_sets_current.get(x).unwrap();

                if *current != new {
                    debug_assert!(current.is_subset(&new)); // new の方が大きくなっているはず
                    *first_sets.get_mut(x).unwrap() = new;
                    changed = true;
                }
            }

            first_sets_current = first_sets.clone();

            if !changed {
                break;
            }
        }

        first_sets
    }

    pub fn calculate_follow_sets(&self) -> HashMap<Rc<NonTerminal>, HashSet<Rc<Terminal>>> {
        let mut follow_sets = self
            .non_terminals
            .iter()
            .map(|x| (x.clone(), HashSet::new()))
            .collect::<HashMap<Rc<NonTerminal>, HashSet<Rc<Terminal>>>>();

        let mut follow_sets_current = follow_sets.clone();

        let nullables = self.calculate_nullables();
        let first_sets = self.calculate_first_sets();

        // production の右辺で出てくるnon_terminalを調べておく
        // x を key とするとき、value には Y -> γxδ なる (Y, δ) を詰め込んだ Vec が入る
        let mut y_and_delta_s = self
            .non_terminals
            .iter()
            .map(|x| (x.clone(), vec![]))
            .collect::<HashMap<Rc<NonTerminal>, Vec<(Rc<NonTerminal>, Vec<Rc<Symbol>>)>>>();
        for (y, rhs_s) in self.productions.iter() {
            for rhs in rhs_s.iter() {
                for i in 0..rhs.len() {
                    let symbol = &rhs[i];
                    if self.non_terminals.contains(symbol) {
                        let delta = rhs[i + 1..rhs.len()].to_vec();
                        y_and_delta_s
                            .get_mut(symbol)
                            .unwrap()
                            .push((y.clone(), delta));
                    }
                }
            }
        }

        // 最小不動点の計算
        loop {
            let mut changed = false;

            for x in self.non_terminals.iter() {
                let y_and_delta_s_for_x = y_and_delta_s.get(x).unwrap();

                let mut new = HashSet::new();

                for (y, delta) in y_and_delta_s_for_x.iter() {
                    let mut first_sets_of_delta = HashSet::new();

                    for symbol in delta.iter() {
                        if self.terminals.contains(symbol) {
                            first_sets_of_delta.insert(symbol.clone());
                        } else {
                            first_sets_of_delta
                                .extend(first_sets.get(symbol).unwrap().iter().cloned());
                        }

                        if !nullables.get(symbol).unwrap_or(&false) {
                            break;
                        }
                    }

                    new.extend(first_sets_of_delta);

                    let delta_is_nullable = delta
                        .iter()
                        .all(|symbol| *nullables.get(symbol).unwrap_or(&false));

                    if delta_is_nullable {
                        new.extend(follow_sets_current.get(y).unwrap().clone());
                    }
                }

                let current = follow_sets_current.get(x).unwrap();
                if *current != new {
                    *follow_sets.get_mut(x).unwrap() = new;
                    changed = true;
                }
            }

            follow_sets_current = follow_sets.clone();

            if !changed {
                break;
            }
        }

        follow_sets
    }

    pub fn calculate_director_sets(
        &self,
    ) -> HashMap<Rc<NonTerminal>, HashMap<Vec<Rc<Symbol>>, HashSet<Rc<Terminal>>>> {
        let nullables = self.calculate_nullables();
        let first_sets = self.calculate_first_sets();
        let follow_sets = self.calculate_follow_sets();

        let mut director_sets = self
            .productions
            .iter()
            .map(|(x, rhs_s)| {
                let hm = rhs_s
                    .iter()
                    .map(|rhs| ((rhs.clone(), HashSet::new())))
                    .collect::<HashMap<Vec<Rc<Symbol>>, HashSet<Rc<Terminal>>>>();

                (x.clone(), hm)
            })
            .collect::<HashMap<Rc<NonTerminal>, HashMap<Vec<Rc<Symbol>>, HashSet<Rc<Terminal>>>>>();

        for (x, rhs_s) in self.productions.iter() {
            for rhs in rhs_s.iter() {
                let mut res = HashSet::new();
                // rhs の First set を求める
                for symbol in rhs.iter() {
                    if self.terminals.contains(symbol) {
                        res.insert(symbol.clone());
                        break;
                    } else {
                        res.extend(first_sets.get(symbol).unwrap().clone());
                        if !nullables.get(symbol).unwrap() {
                            break;
                        }
                    }
                }

                let rhs_is_nullable = rhs
                    .iter()
                    .all(|symbol| *nullables.get(symbol).unwrap_or(&false));
                if rhs_is_nullable {
                    res.extend(follow_sets.get(x).unwrap().clone());
                }

                *director_sets.get_mut(x).unwrap().get_mut(rhs).unwrap() = res;
            }
        }

        director_sets
    }

    pub fn is_ll1(&self) -> bool {
        let director_sets = self.calculate_director_sets();

        for (_, hm) in director_sets.iter() {
            let keys: Vec<_> = hm.keys().collect();

            for i in 0..keys.len() {
                for j in (i + 1)..keys.len() {
                    let set1 = &hm[keys[i]];
                    let set2 = &hm[keys[j]];

                    if !set1.is_disjoint(&set2) {
                        return false;
                    }
                }
            }
        }

        true
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

    #[test]
    fn calculate_first_sets() {
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

        let first_sets = g2.calculate_first_sets();

        let first_sets_expected = [
            ("E'", vec!["+"]),
            ("T", vec!["(", "i"]),
            ("F", vec!["(", "i"]),
            ("E", vec!["(", "i"]),
            ("T'", vec!["*"]),
        ]
        .into_iter()
        .map(|(v, fs)| {
            (
                Rc::new(v.to_string()),
                fs.into_iter()
                    .map(|c| Rc::new(c.to_string()))
                    .collect::<HashSet<_>>(),
            )
        })
        .collect::<HashMap<Rc<NonTerminal>, HashSet<Rc<Terminal>>>>();

        assert_eq!(first_sets, first_sets_expected);
    }

    #[test]
    fn calculate_follow_sets() {
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

        let follow_sets = g2.calculate_follow_sets();

        let follow_sets_expected = [
            ("E", vec![")"]),
            ("E'", vec![")"]),
            ("T", vec!["+", ")"]),
            ("T'", vec!["+", ")"]),
            ("F", vec!["*", "+", ")"]),
        ]
        .into_iter()
        .map(|(v, fs)| {
            (
                Rc::new(v.to_string()),
                fs.into_iter()
                    .map(|c| Rc::new(c.to_string()))
                    .collect::<HashSet<_>>(),
            )
        })
        .collect::<HashMap<Rc<NonTerminal>, HashSet<Rc<Terminal>>>>();

        assert_eq!(follow_sets, follow_sets_expected);
    }

    #[test]
    fn calculate_director_sets() {
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

        let director_sets = g2.calculate_director_sets();

        let director_sets_expected_original = [
            (("E", vec!["T", "E'"]), vec!["(", "i"]),
            (("E'", vec!["+", "T", "E'"]), vec!["+"]),
            (("E'", vec![]), vec![")"]),
            (("T", vec!["F", "T'"]), vec!["(", "i"]),
            (("T'", vec!["*", "F", "T'"]), vec!["*"]),
            (("T'", vec![]), vec!["+", ")"]),
            (("F", vec!["(", "E", ")"]), vec!["("]),
            (("F", vec!["i"]), vec!["i"]),
        ];

        let mut director_sets_expected = HashMap::new();
        for ((x, rhs), ds) in director_sets_expected_original {
            let x = Rc::new(x.to_string());
            let rhs = rhs
                .into_iter()
                .map(|c| Rc::new(c.to_string()))
                .collect::<Vec<_>>();
            let ds = ds
                .into_iter()
                .map(|c| Rc::new(c.to_string()))
                .collect::<HashSet<_>>();

            director_sets_expected
                .entry(x)
                .or_insert_with(HashMap::new)
                .insert(rhs, ds);
        }

        assert_eq!(director_sets, director_sets_expected);
    }

    #[test]
    fn check_ll1() {
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

        assert!(g2.is_ll1());
    }
}
