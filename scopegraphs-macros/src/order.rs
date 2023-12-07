use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use syn::parse::{Parse, ParseStream};
use syn::{braced, Ident, Path, Token};

pub(crate) struct OrderInput {
    type_path: Path,
    _colon: Token![:],
    order: Vec<LabelSetOrder>,
}

pub(crate) struct LabelSetOrder(Vec<LabelSet>);

pub(crate) struct LabelSet(Vec<EdgeOrData>);

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub(crate) enum EdgeOrData {
    Edge(Ident),
    Data,
}

impl Display for EdgeOrData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeOrData::Edge(label) => Display::fmt(&label, f),
            EdgeOrData::Data => write!(f, "$"),
        }
    }
}

impl Parse for OrderInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let type_path = input.parse()?;
        let _colon = input.parse()?;
        let order = input
            .parse_terminated(LabelSetOrder::parse, Token![,])?
            .into_iter()
            .collect();
        Ok(Self {
            type_path,
            _colon,
            order,
        })
    }
}

impl Parse for LabelSetOrder {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut label_set = Vec::<LabelSet>::new();
        loop {
            label_set.push(input.parse()?);
            if input.parse::<Token![<]>().is_err() {
                break;
            }
        }
        Ok(Self(label_set))
    }
}

impl LabelSet {
    fn parse_single(input: ParseStream) -> syn::Result<Self> {
        let val = input.parse()?;
        Ok(LabelSet(vec![val]))
    }

    fn parse_group(input: ParseStream) -> syn::Result<Self> {
        let content;
        braced!(content in input);
        let label_set = content
            .parse_terminated(EdgeOrData::parse, Token![,])?
            .into_iter()
            .collect();
        Ok(Self(label_set))
    }
}

impl Parse for LabelSet {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::token::Brace) {
            Self::parse_group(input)
        } else {
            Self::parse_single(input)
        }
    }
}

impl Parse for EdgeOrData {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![$]) {
            input.parse::<Token![$]>()?;
            Ok(EdgeOrData::Data)
        } else {
            let ident = input.parse()?;
            Ok(EdgeOrData::Edge(ident))
        }
    }
}

impl EdgeOrData {
    fn to_tokens(&self, type_path: &Path) -> TokenStream {
        match self {
            EdgeOrData::Edge(label) => {
                quote!(scopegraphs::resolve::EdgeOrData::Edge(#type_path::#label))
            }
            EdgeOrData::Data => quote!(scopegraphs::resolve::EdgeOrData::Data),
        }
    }
}

enum StrictPartialOrderViolation {
    Symmetric(EdgeOrData),
    Reflexive(EdgeOrData, EdgeOrData),
}

impl Display for StrictPartialOrderViolation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StrictPartialOrderViolation::Symmetric(l) => write!(f, "symmetric: {l} < {l}"),
            StrictPartialOrderViolation::Reflexive(l1, l2) => {
                write!(f, "reflexive: {l1} < {l2} and {l2} < {l1}")
            }
        }
    }
}

fn violations_to_error(violations: Vec<StrictPartialOrderViolation>) -> syn::Error {
    let mut message = "Error: relation should be a strict partial order".to_owned();
    for v in violations {
        message.push_str(&format!("\n  - {v}"))
    }
    syn::Error::new(Span::call_site(), message)
}

impl OrderInput {
    pub(crate) fn compile(&self) -> TokenStream {
        let mut forward: HashMap<&EdgeOrData, HashSet<&EdgeOrData>> = HashMap::new();
        let mut backward: HashMap<&EdgeOrData, HashSet<&EdgeOrData>> = HashMap::new();

        let mut errors: Vec<StrictPartialOrderViolation> = Vec::new();

        for order in &self.order {
            for entry_slice in order.0.windows(2) {
                for lower_elem in entry_slice[0].0.iter() {
                    for upper_elem in entry_slice[1].0.iter() {
                        let mut queue = VecDeque::from([(lower_elem, upper_elem)]);
                        while let Some((lower, upper)) = queue.pop_back() {
                            // Emit error if relation is symmetric
                            if lower == upper {
                                errors.push(StrictPartialOrderViolation::Symmetric(lower.clone()));
                                continue;
                            }

                            // emit error if relation is reflexive
                            if forward.entry(upper).or_default().contains(&lower) {
                                errors.push(StrictPartialOrderViolation::Reflexive(
                                    upper.clone(),
                                    lower.clone(),
                                ));
                                continue;
                            }

                            // insert forward entry and queue transitive elements for transitive closure computation
                            if forward.entry(lower).or_default().insert(upper) {
                                for trans_upper in forward.entry(upper).or_default().iter() {
                                    if *trans_upper != upper {
                                        queue.push_back((lower, trans_upper));
                                    }
                                }
                            }

                            // insert backward entry and queue transitive elements for transitive closure computation
                            if backward.entry(upper).or_default().insert(lower) {
                                for trans_lower in backward.entry(lower).or_default().iter() {
                                    if *trans_lower != lower {
                                        queue.push_back((trans_lower, upper));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        let flatten = forward
            .iter()
            .flat_map(|(lower, uppers)| uppers.iter().map(|upper| (*lower, *upper)))
            .collect::<Vec<(&EdgeOrData, &EdgeOrData)>>();

        let type_path = &self.type_path;

        let lhs = flatten
            .iter()
            .map(|x| x.0.to_tokens(type_path))
            .collect::<Vec<_>>();
        let rhs = flatten
            .iter()
            .map(|x| x.1.to_tokens(type_path))
            .collect::<Vec<_>>();

        let label_order_function = quote!(
            |&l1: &scopegraphs::resolve::EdgeOrData<#type_path> , &l2: &scopegraphs::resolve::EdgeOrData<#type_path> | {
                matches!((l1, l2), #((#lhs, #rhs))|*)
            }
        );

        if !errors.is_empty() {
            let error = violations_to_error(errors).into_compile_error();
            quote!({
                #error;
                #label_order_function
            })
        } else {
            label_order_function
        }
    }
}
