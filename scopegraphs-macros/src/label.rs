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

    for variant in variants {
        if !variant.fields.is_empty() {
            return quote_spanned!(
                variant.span() => compile_error!("cannot derive Label for an enum with fields on variants");
            )
            .into();
        }

        variant_names.push(variant.ident);
    }

    let name = input.ident;
    quote! {
        impl scopegraphs::Label for #name {
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
