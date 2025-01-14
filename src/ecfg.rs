use core::panic;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::CFG;

type NonTerminal = String;
type Terminal = String;
type Symbol = String;

#[derive(Debug)]
pub struct ECFG {
    terminals: HashSet<Rc<Terminal>>,
    non_terminals: HashSet<Rc<NonTerminal>>,
    productions: HashMap<Rc<String>, Vec<Rc<String>>>,
    start_symbol: Rc<Terminal>,
    cfg: CFG,
}

impl ECFG {
    pub fn is_ell1(&self) -> bool {
        self.cfg.is_ll1()
    }

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

        let productions: HashSet<(String, Vec<String>)> = productions
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
        for x in non_terminals {
            let tmp = Rc::new(x);
            nt_s.insert(tmp);
        }

        let special_symbols: HashSet<Rc<String>> = ["\\{", "\\}", "\\(", "\\)", "\\|"]
            .into_iter()
            .map(|v| Rc::new(v.to_string()))
            .collect::<HashSet<Rc<String>>>();

        let mut p_s = HashMap::new();

        let vertical_bar = Rc::new("\\|".to_string());

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

                    if let Some(rhs_element) = special_symbols.get(&p_symbol) {
                        rhs.push(rhs_element.clone());
                        continue;
                    }

                    panic!(
                        "'{}' is neither included in non_terminals nor terminals.",
                        p_symbol
                    );
                }
                match p_s.entry(lhs.clone()) {
                    Entry::Occupied(mut entry) => {
                        let v: &mut Vec<Rc<String>> = entry.get_mut();
                        v.push(vertical_bar.clone());
                        v.extend(rhs);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(rhs);
                    }
                }
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

        let cfg = Self::parse_productions(&t_s, &nt_s, &p_s, &s);

        ECFG {
            terminals: t_s,
            non_terminals: nt_s,
            productions: p_s,
            start_symbol: s,
            cfg,
        }
    }

    fn parse_productions(
        terminals: &HashSet<Rc<Terminal>>,
        non_terminals: &HashSet<Rc<NonTerminal>>,
        productions: &HashMap<Rc<String>, Vec<Rc<String>>>,
        start_symbol: &Rc<NonTerminal>,
    ) -> CFG {
        let t_s = terminals.iter().map(|nt| nt.as_ref().clone()).collect();
        let nt_s = non_terminals.iter().map(|nt| nt.as_ref().clone()).collect();
        let p_s = productions
            .iter()
            .map(|(lhs, rhs)| {
                (
                    lhs.as_ref().clone(),
                    rhs.iter().map(|symbol| symbol.as_ref().clone()).collect(),
                )
            })
            .collect();
        let s = start_symbol.as_ref().clone();

        let (nt_s, p_s) = Self::parse_parentheses(nt_s, p_s);
        let (nt_s, p_s) = Self::parse_braces(nt_s, p_s);
        let p_s = Self::parse_vertical_bar(p_s);

        CFG::new(t_s, nt_s, p_s, s)
    }

    fn parse_parentheses(
        mut non_terminals: HashSet<NonTerminal>,
        productions: HashSet<(String, Vec<String>)>,
    ) -> (HashSet<NonTerminal>, HashSet<(String, Vec<String>)>) {
        let base = Self::get_unused_non_terminal_base(&non_terminals, "P");
        let mut get_new_non_terminal = {
            let mut count = 0;
            move || {
                count += 1;
                let mut new_non_terminal = base.clone();
                new_non_terminal.push('_');
                new_non_terminal.push_str(&count.to_string());
                new_non_terminal
            }
        };

        let mut new_productions = HashSet::new();

        for (lhs, rhs) in productions.into_iter() {
            let mut stack = vec![];

            for s in rhs {
                stack.push(s.clone());
                if s == "\\)" {
                    let new_non_terminal = get_new_non_terminal();
                    let mut new_rhs = vec![];

                    let mut should_panic = true;

                    while let Some(t) = stack.pop() {
                        if t != "\\)" && t != "\\(" {
                            new_rhs.push(t.clone());
                        }
                        if t == "\\(" {
                            stack.push(new_non_terminal.clone());
                            should_panic = false;
                            break;
                        }
                    }
                    if should_panic {
                        panic!("Parenthesis not match");
                    }

                    new_rhs.reverse();

                    non_terminals.insert(new_non_terminal.clone());
                    new_productions.insert((new_non_terminal, new_rhs));
                }
            }

            if stack.contains(&"\\(".to_string()) || stack.contains(&"\\)".to_string()) {
                panic!("Parenthesis not match");
            }

            new_productions.insert((lhs, stack));
        }

        (non_terminals, new_productions)
    }

    fn parse_braces(
        mut non_terminals: HashSet<Terminal>,
        productions: HashSet<(String, Vec<String>)>,
    ) -> (HashSet<Terminal>, HashSet<(String, Vec<String>)>) {
        let base = Self::get_unused_non_terminal_base(&non_terminals, "B");
        let mut get_new_non_terminal = {
            let mut count = 0;
            move || {
                count += 1;
                let mut new_non_terminal = base.clone();
                new_non_terminal.push('_');
                new_non_terminal.push_str(&count.to_string());
                new_non_terminal
            }
        };

        let mut new_productions = HashSet::new();

        for (lhs, rhs) in productions.into_iter() {
            let mut stack = vec![];

            for s in rhs {
                stack.push(s.clone());
                if s == "\\}" {
                    let new_non_terminal = get_new_non_terminal();
                    let mut new_rhs = vec![];

                    let mut should_panic = true;

                    while let Some(t) = stack.pop() {
                        if t != "\\}" && t != "\\{" {
                            new_rhs.push(t.clone());
                        }
                        if t == "\\{" {
                            stack.push(new_non_terminal.clone());
                            should_panic = false;
                            break;
                        }
                    }
                    if should_panic {
                        panic!("Braces not match");
                    }

                    new_rhs.reverse();
                    new_rhs.push(new_non_terminal.clone());

                    non_terminals.insert(new_non_terminal.clone());
                    new_productions.insert((new_non_terminal.clone(), new_rhs));
                    new_productions.insert((new_non_terminal, vec![]));
                }
            }

            if stack.contains(&"\\{".to_string()) || stack.contains(&"\\}".to_string()) {
                panic!("Parenthesis not match");
            }

            new_productions.insert((lhs, stack));
        }

        (non_terminals, new_productions)
    }

    fn parse_vertical_bar(
        productions: HashSet<(String, Vec<String>)>,
    ) -> HashSet<(String, Vec<String>)> {
        let mut new_productions = HashSet::new();

        for (lhs, rhs) in productions.into_iter() {
            let mut stack = vec![];

            for s in rhs {
                stack.push(s.clone());

                if s == "\\|" {
                    stack.pop();

                    new_productions.insert((lhs.clone(), stack));

                    stack = vec![];
                }
            }

            new_productions.insert((lhs, stack));
        }

        new_productions
    }

    fn get_unused_non_terminal_base(non_terminals: &HashSet<Terminal>, s: &str) -> String {
        let mut len = 0;
        for nt in non_terminals.iter() {
            len = len.max(nt.len());
        }

        let mut res = String::from(s);
        for _ in 0..len {
            res.push('_');
        }

        res
    }

    pub fn calculate_nullable(&self, s: &Vec<Symbol>) -> bool {
        self.validate_symbols(s);

        self._calculate_nullable_inner(s)
    }

    pub fn calculate_first_set(&self, s: &Vec<Symbol>) -> HashSet<Symbol> {
        self.validate_symbols(s);

        self._calculate_first_set_inner(s)
    }

    pub fn calculate_follow_set(&self, s: &Symbol) -> HashSet<Symbol> {
        if !self.non_terminals.contains(s) {
            panic!("{} is not a non terminal", s);
        }

        let original_follow_set = &self.cfg.calculate_follow_sets()[s];
        let follow_set = original_follow_set
            .iter()
            .map(|v| (**v).clone())
            .collect::<HashSet<_>>();

        follow_set
    }

    /// s -> α | β という productions を持つ s に対して、s -> α と s -> β のディレクターを返す
    pub fn calculate_director_set(&self, s: &NonTerminal) -> HashMap<Vec<Symbol>, HashSet<Symbol>> {
        if !self.non_terminals.contains(s) {
            panic!("{} is not a non terminal.", s);
        }

        let follow_set = self.calculate_follow_set(s);
        let rhs_s = self.productions[&Rc::new(s.clone())]
            .iter()
            .map(|rc| (**rc).clone())
            .collect::<Vec<Symbol>>();

        let broken_rhs_s = Self::break_regular_expression_by_vertical_bar(&rhs_s);

        let mut res = HashMap::new();
        for rhs in broken_rhs_s.iter() {
            let mut director_set = self.calculate_first_set(rhs);
            if self.calculate_nullable(rhs) {
                director_set.extend(follow_set.clone());
            }
            res.insert(rhs.clone(), director_set);
        }

        res
    }

    fn validate_symbols(&self, s: &Vec<Symbol>) {
        for ss in s.iter() {
            if !(["\\{", "\\}", "\\(", "\\)", "\\|"]
                .into_iter()
                .map(|sss| sss.to_string())
                .collect::<Vec<String>>()
                .contains(ss)
                || self.non_terminals.contains(ss)
                || self.terminals.contains(ss))
            {
                panic!("\"{}\" is neither in terminals nor non_terminals.", ss);
            }
        }
    }

    fn _calculate_nullable_inner(&self, s: &[Symbol]) -> bool {
        let n = s.len();

        if n == 0 {
            return true;
        }

        if n == 1 {
            if self.non_terminals.contains(&s[0]) {
                let original_nullables = &self.cfg.calculate_nullables();
                return original_nullables[&s[0]];
            }

            if self.terminals.contains(&s[0]) {
                return false;
            }

            panic!(
                "parse failed: \"{}\" is neither in terminals nor non_teminals",
                s[0]
            );
        }

        let broken_regular_expressions_by_vertical_bar =
            Self::break_regular_expression_by_vertical_bar(s);

        if broken_regular_expressions_by_vertical_bar.len() != 1 {
            let mut is_nullable = false;
            for ss in broken_regular_expressions_by_vertical_bar.iter() {
                is_nullable = is_nullable || self._calculate_nullable_inner(ss);
            }

            return is_nullable;
        }

        let ss = &broken_regular_expressions_by_vertical_bar[0];

        let broken_regular_expression_by_concatenation =
            Self::break_regular_expression_by_concatenation(ss);

        if broken_regular_expression_by_concatenation.len() != 1 {
            let mut is_nullable = true;
            for sss in broken_regular_expression_by_concatenation.iter() {
                is_nullable = is_nullable && self._calculate_nullable_inner(sss);
            }

            return is_nullable;
        }

        let sss = &broken_regular_expression_by_concatenation[0];

        if sss[0] == "\\{" && ss[n - 1] == "\\}" {
            return true;
        }

        if sss[0] == "\\(" && ss[n - 1] == "\\)" {
            return self._calculate_nullable_inner(&s[1..n - 1]);
        }

        panic!("something wrong occured in calculating nullable: {:?}?", s);
    }

    fn _calculate_first_set_inner(&self, s: &[Symbol]) -> HashSet<Symbol> {
        let n = s.len();

        if n == 0 {
            return HashSet::new();
        }

        if n == 1 {
            if self.non_terminals.contains(&s[0]) {
                let original_first_set = &self.cfg.calculate_first_sets()[&s[0]];

                let first_set = original_first_set
                    .iter()
                    .map(|v| (**v).clone())
                    .collect::<HashSet<_>>();

                return first_set;
            }

            if self.terminals.contains(&s[0]) {
                let mut first_set = HashSet::new();
                first_set.insert(s[0].clone());
                return first_set;
            }

            panic!(
                "parse failed: \"{}\" is neither in terminals nor non_teminals",
                s[0]
            );
        }

        let broken_regular_expressions_by_vertical_bar =
            Self::break_regular_expression_by_vertical_bar(s);

        if broken_regular_expressions_by_vertical_bar.len() != 1 {
            let mut first_set = HashSet::new();
            for ss in broken_regular_expressions_by_vertical_bar.iter() {
                first_set.extend(self._calculate_first_set_inner(ss));
            }

            return first_set;
        }

        let ss = &broken_regular_expressions_by_vertical_bar[0];

        let broken_regular_expression_by_concatenation =
            Self::break_regular_expression_by_concatenation(ss);

        if broken_regular_expression_by_concatenation.len() != 1 {
            let mut first_set = HashSet::new();
            for ss in broken_regular_expression_by_concatenation.iter() {
                first_set.extend(self._calculate_first_set_inner(ss));
                if !self.calculate_nullable(ss) {
                    break;
                }
            }

            return first_set;
        }

        let sss = &broken_regular_expression_by_concatenation[0];

        if sss[0] == "\\{" && ss[n - 1] == "\\}" {
            return self._calculate_first_set_inner(&s[1..n - 1]);
        }

        if sss[0] == "\\(" && ss[n - 1] == "\\)" {
            return self._calculate_first_set_inner(&s[1..n - 1]);
        }

        panic!("something wrong occured in calculating first set: {:?}?", s);
    }

    /// 一番外側の | で分解
    ///
    /// # Examples
    ///
    /// {hoge} | fuga | {piyo1 | piyo2} を {hoge}, fuga, {piyo1 | piyo2} に分解
    fn break_regular_expression_by_vertical_bar(s: &[Symbol]) -> Vec<Vec<Symbol>> {
        let mut brace_count = 0;
        let mut parenthesis_count = 0;

        let mut stack = vec![];

        let mut res = vec![];

        for symbol in s.iter() {
            if symbol == "\\|" {
                if parenthesis_count == 0 && brace_count == 0 {
                    res.push(stack);
                    stack = vec![];
                    continue;
                }
            }

            stack.push(symbol.clone());

            if symbol == "\\{" {
                brace_count += 1;
            }
            if symbol == "\\}" {
                brace_count -= 1;
            }
            if symbol == "\\(" {
                parenthesis_count += 1;
            }
            if symbol == "\\)" {
                parenthesis_count -= 1;
            }
        }

        res.push(stack);

        if !(parenthesis_count == 0 && brace_count == 0) {
            panic!("parse failed in breaking by vertical bar");
        }

        res
    }

    /// 一番外側のシンボルの結合で分解
    ///
    /// # Examples
    ///
    /// {hoge} fuga (piyo1 | piyo2) を {hoge}, fuga, (piyo1 | piyo2) に分解
    ///
    /// # Remark
    ///
    /// 一番外側の | を無くした後のシンボル列に用いる
    fn break_regular_expression_by_concatenation(s: &[Symbol]) -> Vec<Vec<Symbol>> {
        let mut stack = vec![];

        let mut res = vec![];

        let mut brace_count = 0;
        let mut parenthesis_count = 0;

        for symbol in s.iter() {
            if symbol == "\\(" {
                parenthesis_count += 1;
            } else if symbol == "\\{" {
                brace_count += 1;
            } else if symbol == "\\)" {
                parenthesis_count -= 1;
            } else if symbol == "\\}" {
                brace_count -= 1;
            }
            stack.push(symbol.clone());

            if parenthesis_count == 0 && brace_count == 0 {
                res.push(stack);
                stack = vec![];
            }
        }

        if !(parenthesis_count == 0 && brace_count == 0) {
            panic!("parse failed in breaking by concatenation");
        }

        res
    }
}

#[cfg(test)]
mod rrcfg_test {
    use super::*;

    #[test]
    fn check_ell1() {
        {
            let terminals = vec!["+", "*", "i", "(", ")"];

            let non_terminals = vec!["E", "T", "F"];

            let productions = vec![
                ("E", vec!["T", "\\{", "+", "T", "\\}"]),
                ("T", vec!["F", "\\{", "*", "F", "\\}"]),
                ("F", vec!["(", "E", ")", "\\|", "i"]),
            ];

            let start_symbol = "E";

            let g3 = ECFG::new(terminals, non_terminals, productions, start_symbol);

            println!("{:?}", g3.cfg);

            assert!(g3.is_ell1());
        }

        {
            let terminals = vec!["+", "*", "i", "(", ")"];

            let non_terminals = vec!["E", "T", "F"];

            let productions = vec![
                ("E", vec!["T", "\\{", "\\{", "+", "T", "\\}", "\\}"]),
                ("T", vec!["F", "\\{", "*", "F", "\\}"]),
                ("F", vec!["(", "E", ")", "\\|", "i"]),
            ];

            let start_symbol = "E";

            let g3 = ECFG::new(terminals, non_terminals, productions, start_symbol);

            println!("{:?}", g3.cfg);
            println!("{:?}", g3.cfg.calculate_follow_sets());

            assert!(!g3.is_ell1());
        }
    }

    #[test]
    fn check_break_rhs_by_vertical_bar() {
        let target = vec![
            "(", "\\{", "E", "\\|", "F", "\\(", "A", "\\|", "E", "\\)", "\\}", ")", "\\|", "i",
        ];
        let target = target
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();

        let expected = [
            vec![
                "(", "\\{", "E", "\\|", "F", "\\(", "A", "\\|", "E", "\\)", "\\}", ")",
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect(),
            vec!["i".to_string()],
        ];

        assert_eq!(
            ECFG::break_regular_expression_by_vertical_bar(&target),
            expected
        );
    }

    // テストが弱い
    #[test]
    fn check_nullables_() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "T", "F"];

        let productions = vec![
            ("E", vec!["T", "\\{", "+", "T", "\\}"]),
            ("T", vec!["F", "\\{", "*", "F", "\\}"]),
            ("F", vec!["(", "E", ")", "\\|", "i"]),
        ];

        let start_symbol = "E";

        let g3 = ECFG::new(terminals, non_terminals, productions, start_symbol);

        assert_eq!(
            g3.calculate_nullable(&vec!["\\{".to_string(), "E".to_string(), "\\}".to_string()]),
            true
        );
    }

    // テストが弱い
    #[test]
    fn check_first_set_() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "T", "F"];

        let productions = vec![
            ("E", vec!["T", "\\{", "+", "T", "\\}"]),
            ("T", vec!["F", "\\{", "*", "F", "\\}"]),
            ("F", vec!["(", "E", ")", "\\|", "i"]),
        ];

        let start_symbol = "E";

        let g3 = ECFG::new(terminals, non_terminals, productions, start_symbol);

        assert_eq!(
            g3.calculate_first_set(&vec!["+".to_string(), "T".to_string()]),
            vec!["+".to_string()]
                .into_iter()
                .collect::<HashSet<String>>()
        );

        assert_eq!(
            g3.calculate_first_set(&vec!["T".to_string()]),
            vec!["i".to_string(), "(".to_string()]
                .into_iter()
                .collect::<HashSet<String>>()
        );

        assert_eq!(
            g3.calculate_first_set(&vec![
                "\\{".to_string(),
                "*".to_string(),
                "F".to_string(),
                "\\}".to_string()
            ]),
            vec!["*".to_string()]
                .into_iter()
                .collect::<HashSet<String>>()
        );
    }

    // テストが弱い
    #[test]
    fn check_director_set_() {
        let terminals = vec!["+", "*", "i", "(", ")"];

        let non_terminals = vec!["E", "T", "F"];

        let productions = vec![
            ("E", vec!["T", "\\{", "+", "T", "\\}"]),
            ("T", vec!["F", "\\{", "*", "F", "\\}"]),
            ("F", vec!["(", "E", ")", "\\|", "i"]),
        ];

        let start_symbol = "E";

        let g3 = ECFG::new(terminals, non_terminals, productions, start_symbol);

        let d1 = g3.calculate_director_set(&"E".to_string());
        let mut hs = HashSet::new();
        hs.insert("(".to_string());
        hs.insert("i".to_string());
        let mut d1_expected = HashMap::new();
        d1_expected.insert(
            vec![
                "T".to_string(),
                "\\{".to_string(),
                "+".to_string(),
                "T".to_string(),
                "\\}".to_string(),
            ],
            hs,
        );

        assert_eq!(d1, d1_expected);
    }
}
