use std::rc::Rc;

use proc_macro::TokenStream;
use scopegraphs_regular_expressions::Regex;
use syn::parse::{Parse, ParseStream};
use syn::{Attribute, Ident, Meta, Token, Type};

#[cfg(feature = "dot")]
use std::fs::File;
#[cfg(feature = "dot")]
use syn::{Expr, ExprLit, Lit, MetaNameValue};

pub(crate) struct RegexInput {
    attrs: Vec<Attribute>,
    _type: Token![type],
    name: Ident,
    _open: Token![<],
    alphabet_type: Type,
    _close: Token![>],
    _equals: Token![=],
    regex: Regex,
    errors: Vec<syn::Error>,
}

impl Parse for RegexInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let _type = input.parse()?;
        let name = input.parse()?;
        let _open = input.parse()?;
        let alphabet_type = input.parse()?;
        let _close = input.parse()?;
        let _equals = input.parse()?;
        let (regex, errors) = match input.parse() {
            Ok(re) => (re, vec![]),
            Err(err) => (Regex::Complement(Rc::new(Regex::EmptySet)), vec![err]),
        };
        Ok(Self {
            attrs,
            _type,
            name,
            _open,
            alphabet_type,
            _close,
            _equals,
            regex,
            errors,
        })
    }
}

impl RegexInput {
    pub fn compile(self) -> TokenStream {
        let mut errors = self.errors;
        #[cfg(feature = "dot")]
        let mut graph = None;

        for i in self.attrs {
            let attr = i.meta.clone();
            match attr {
                #[cfg(feature = "dot")]
                Meta::NameValue(MetaNameValue {
                    path,
                    value:
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }),
                    ..
                }) if path.is_ident("graph") => {
                    graph = Some(s);
                }
                i => errors.push(syn::Error::new_spanned(i, "unexpected attribute")),
            }
        }

        let compiled = self.regex.compile();

        #[cfg(feature = "dot")]
        if let Some(path) = graph {
            let path = path.value();
            let mut f = File::create(&path)
                .unwrap_or_else(|e| panic!("can't open dot file for graphing at {path}: {e}"));
            compiled
                .output_dot(&mut f)
                .unwrap_or_else(|e| panic!("failed while graphing at {path}: {e}"));
        }

        compiled
            .emit(&self.name, &self.alphabet_type, errors)
            .into()
    }
}
