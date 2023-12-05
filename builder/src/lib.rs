extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data::Struct, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    // println!("{:?}", input.clone());
    // eprintln!("INPUT: {:#?}", input);
    let struct_name = input.ident;
    let builder_name = syn::Ident::new(&format!("{}Builder", struct_name), struct_name.span());
    let vis = syn::Visibility::Inherited;
    let fields: syn::DataStruct = match input.data {
        Struct(fields) => fields,
        other => {
            panic!("expected syn::Data::Struct but got {:?}", other)
        }
    };

    let field_names = if let syn::Fields::Named(f) = fields.fields.clone() {
        f.named
            .iter()
            .map(|field| field.ident.clone())
            .filter(|named_f| named_f.is_some())
            .collect()
    } else {
        Vec::new()
    };
    let field_types = if let syn::Fields::Named(f) = fields.fields.clone() {
        f.named.iter().map(|field| field.ty.clone()).collect()
    } else {
        Vec::new()
    };


    let generated_tokens = quote! {
        #vis struct #builder_name {
            #(#field_names: Option<#field_types>),*
        }

        impl #struct_name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#field_names: None),*
                }
            }
        }

        impl #builder_name {
            #(pub fn #field_names(&mut self, #field_names: #field_types) -> &mut Self {
                self.#field_names = Some(#field_names);
                self
            })
            *

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    if self.#field_names.is_none() {
                        // return Err(format!("field `{}` must be set before calling .build", self.#field_names).into());
                        return Err("field must be set before calling .build".into());
                    }
                )*
                // self.#field_names.ok_or("field must be set before calling .build")?
                // Ok(#struct_name {
                //     #(#field_names: self.#field_names.ok_or("field must be set before calling .build")?),*
                // })
                Ok(#struct_name{
                    #(#field_names: self.#field_names.clone().unwrap()),*
                })
            }
        }
    };
    // this is a proc_macro2 TokenStream but it can be
    // converted into a "normal" TokenStream using into
    let generated_tokens = generated_tokens.into();
    eprintln!("TOKENS: {}", generated_tokens);
    return generated_tokens;
}
