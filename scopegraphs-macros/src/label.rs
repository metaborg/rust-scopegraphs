use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Data, DataEnum, DataStruct, DataUnion, DeriveInput};

pub fn impl_label(input: DeriveInput) -> TokenStream {
    let DataEnum { variants, .. } = match input.data {
        Data::Struct(DataStruct { struct_token, .. }) => {
            return quote_spanned!(
                struct_token.span => compile_error!("cannot derive Label for a struct");
            )
            .into()
        }
        Data::Union(DataUnion { union_token, .. }) => {
            return quote_spanned!(
                union_token.span => compile_error!("cannot derive Label for a union");
            )
            .into()
        }
        Data::Enum(e) => e,
    };

    if !input.generics.params.is_empty() {
        return quote_spanned!(
            input.generics.span() => compile_error!("cannot derive Label for a generic enum");
        )
        .into();
    }

    let mut variant_names = Vec::new();
    let mut variant_numbers = Vec::new();

    for (num, variant) in variants.into_iter().enumerate() {
        if !variant.fields.is_empty() {
            return quote_spanned!(
                variant.span() => compile_error!("cannot derive Label for an enum with fields on variants");
            )
            .into();
        }

        if variant.discriminant.is_some() {
            return quote_spanned!(
                variant.span() => compile_error!("cannot derive Label for an enum with custom discriminated variants. Use `Label::to_usize()`");
            )
                .into();
        }

        variant_names.push(variant.ident);
        variant_numbers.push(num);
    }
    let num_variants = variant_numbers.len();

    let name = input.ident;
    quote! {
        unsafe impl scopegraphs::Label for #name {
            type Array<T> = [T; #num_variants];

            fn to_usize(&self) -> usize {
                match *self {
                    #(
                        Self::#variant_names => #variant_numbers
                    ),*
                }
            }

            fn iter() -> impl Iterator<Item = Self> {
                [
                    #(
                        Self::#variant_names
                    ),*
                ].into_iter()
            }

            fn iter_ref() -> impl Iterator<Item = &'static Self> {
                [
                    #(
                        &Self::#variant_names
                    ),*
                ].into_iter()
            }
        }
    }
    .into()
}
