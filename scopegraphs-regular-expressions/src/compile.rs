use crate::regex::Symbol;
use crate::Regex;
use std::collections::{HashSet, VecDeque};

pub struct CompiledRegex<'a> {
    regex: &'a Regex,
}

struct RegexCompiler<'a> {
    alphabet: HashSet<&'a Symbol>,
    regex: &'a Regex,
}

impl<'a> RegexCompiler<'a> {
    fn new(regex: &'a Regex) -> Self {
        Self {
            alphabet: regex.alphabet(),
            regex,
        }
    }

    fn step(&self, r: &Regex, work_list: &mut VecDeque<&'a Regex>) {}

    fn compile(self) -> CompiledRegex<'a> {
        let mut work_list = VecDeque::new();

        work_list.push_back(&Regex::Empty);
        work_list.push_back(self.regex);

        while let Some(r) = work_list.pop_front() {
            self.step(r, &mut work_list);
        }

        CompiledRegex { regex: self.regex }
    }
}

impl Regex {
    pub fn compile(&self) -> CompiledRegex {
        let compiler = RegexCompiler::new(self);
        compiler.compile()
    }
}
