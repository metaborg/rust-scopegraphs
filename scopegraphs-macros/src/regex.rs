use proc_macro::TokenStream;
use quote::quote_spanned;
use scopegraphs_regular_expressions::Regex;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
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
}

impl Parse for RegexInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            _type: input.parse()?,
            name: input.parse()?,
            _open: input.parse()?,
            alphabet_type: input.parse()?,
            _close: input.parse()?,
            _equals: input.parse()?,
            regex: input.parse()?,
        })
    }
}

impl RegexInput {
    pub fn compile(self) -> TokenStream {
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
                i => {
                    return quote_spanned!(i.span() => compile_error!("unexpected attribute");)
                        .into();
                }
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

        compiled.emit(&self.name, &self.alphabet_type).into()
    }
}
