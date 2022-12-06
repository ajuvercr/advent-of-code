extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DataEnum, DeriveInput};

#[proc_macro_derive(Parse)]
pub fn derive_answer_fn(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    match derive(input) {
        Ok(t) => t.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn derive_fields<'a, IT: 'a, I: Iterator<Item = &'a syn::Field>>(
    fields: &'a IT,
) -> proc_macro2::TokenStream
where
    &'a IT: IntoIterator<Item = &'a syn::Field, IntoIter = I>,
{
    let idents = fields.into_iter().map(|x| &x.ident);
    let colors = fields.into_iter().map(|x| x.colon_token);
    let tys = fields.into_iter().map(|x| &x.ty);
    quote!(
        #(#idents #colors <#tys as crate::parser::Parse>::parse(cur)?,)*
    )
}

fn impl_fields(fields: &syn::Fields) -> proc_macro2::TokenStream {
    let fields_derive = derive_fields(fields);

    match fields {
        syn::Fields::Named(_) => quote!( { #fields_derive }),
        syn::Fields::Unnamed(_) => quote!( ( #fields_derive )),
        syn::Fields::Unit => quote!(),
    }
}

fn derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let this = match input.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            let fields = impl_fields(&fields);
            quote! {
                Some(Self
                    #fields
               )
            }
        }
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let options: proc_macro2::TokenStream = variants
                .iter()
                .map(|var| {
                    let fields = impl_fields(&var.fields);
                    let id = &var.ident;
                    let ids = id.to_string();

                    quote! {
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
                            return Some(Self::#id #fields);
                        }
                    }
                })
                .collect();

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
