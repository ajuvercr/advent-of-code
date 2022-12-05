extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, DataEnum, DeriveInput};

#[proc_macro_derive(Parse)]
pub fn derive_answer_fn(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    match derive(input) {
        Ok(t) => t.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn derive_fields(fields: &Punctuated<syn::Field, syn::token::Comma>) -> proc_macro2::TokenStream {
    let idents = fields.iter().map(|x| &x.ident);
    let colors = fields.iter().map(|x| x.colon_token);
    let tys = fields.iter().map(|x| &x.ty);
    quote!(
        #(#idents #colors <#tys as crate::parser::Parse>::parse(cur)?,)*
    )
}

fn derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let this = match input.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(fields),
            ..
        }) => {
            let fields = derive_fields(&fields.named);
            quote! {
                Some(Self {
                    #fields
                })
            }
        }
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Unnamed(fields),
            ..
        }) => {
            let fields = derive_fields(&fields.unnamed);
            quote! {
                Some(Self (
                    #fields
                ))
            }
        }
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Unit,
            ..
        }) => quote! {
            Some(Self)
        },
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let options =
                variants
                    .iter()
                    .try_fold(proc_macro2::TokenStream::new(), |mut acc, var| {
                        if var.fields.iter().next().is_some() {
                            return Err(syn::parse::Error::new(
                                var.ident.span(),
                                "No fields allowed",
                            ));
                        }

                        let id = &var.ident;
                        let ids = id.to_string();

                        acc.extend(
                            [quote! {
                                let id = #ids;
                                let mut chars = id.bytes();

                                let eq = |&c: &u8| {
                                    if let Some(x) = chars.next() {
                                        c.eq_ignore_case(x)
                                    } else {
                                        false
                                    }
                                };
                                let matched = cur.next_while_slice(eq);
                                if matched.len() == id.len() {
                                    return Some(Self::#id);
                                }
                            }]
                            .into_iter(),
                        );
                        Ok(acc)
                    })?;

            quote! {
                use crate::parser::U8Helper as _;
                #options
                None
            }
        }
        syn::Data::Union(u) => {
            return Err(syn::parse::Error::new(
                u.union_token.span,
                "Only struct is supported",
            ))
        }
    };

    let gen = input.generics;
    let ident = input.ident;

    let constraints = gen.type_params();

    let gen_constraints = quote! {
        #(#constraints: crate::parser::Parse<'a>)*
    };

    Ok(quote!(
    impl<'a> #gen crate::parser::Parse<'a> for #ident #gen where #gen_constraints {
        fn parse(cur: &mut crate::parser::Cursor) -> Option<Self> {
            use crate::parser::Parse as _;
            #this
        }
    }))
}
