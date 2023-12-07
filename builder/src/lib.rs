extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data::Struct, DeriveInput, GenericArgument, Field, Type};

#[proc_macro_derive(Builder)]
pub fn derive(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    eprintln!("INPUT: {:#?}", input);
    let struct_name = input.ident;
    let builder_name = syn::Ident::new(&format!("{}Builder", struct_name), struct_name.span());
    let vis = syn::Visibility::Inherited;
    let fields: syn::DataStruct = match input.data {
        Struct(fields) => fields,
        other => {
            panic!("expected syn::Data::Struct but got {:?}", other)
        }
    };

    let struct_fields = parse_struct_fields(fields);

    let (requried_field_names, requried_field_types): (Vec<syn::Ident>, Vec<syn::Type>) =
        struct_fields
            .iter()
            .filter(|(.., option_type)| option_type.is_none())
            .map(|(field, __)| (field.ident.clone().unwrap(), field.ty.clone()))
            .unzip();
    let (option_field_names, option_field_types): (Vec<syn::Ident>, Vec<syn::Type>) =
        struct_fields
            .iter()
            .filter(|(.., option_type)| option_type.is_some())
            .map(|(field, option_type)| (field.ident.clone().unwrap(), option_type.clone().unwrap()))
            .unzip();

    let setters = quote!(
        #(
            pub fn #requried_field_names(&mut self, #requried_field_names: #requried_field_types) -> &mut Self {
                self.#requried_field_names = Some(#requried_field_names);
                self
            }
        )
        *
        #(
            pub fn #option_field_names(&mut self, #option_field_names: #option_field_types) -> &mut Self {
                self.#option_field_names = Some(#option_field_names);
                self
            }
        )
        *
    );

    let builder_struct_def = quote!(
        #vis struct #builder_name {
            #(#requried_field_names: Option<#requried_field_types>),*
            #(,#option_field_names: Option<#option_field_types>),*
        }
    );

    let builder_constructor = quote!(
        impl #struct_name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#requried_field_names: None),*
                    #(,#option_field_names: None),*
                }
            }
        }
    );

    let error_msg: Vec<String> = requried_field_names
        .iter()
        .map(|fname| {
            format!(
                "required field `{}` must be set before calling .build()",
                fname
            )
        })
        .collect();
    let assert_required_fields_are_set = quote!(
        #(
            if self.#requried_field_names.is_none() {
                return Err(#error_msg.into());
            }
        )*
    );

    let builder_impl = quote!(
        impl #builder_name {
            #setters
            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #assert_required_fields_are_set
                Ok(#struct_name{
                    #(#requried_field_names: self.#requried_field_names.clone().unwrap()),*
                    #(,#option_field_names: if self.#option_field_names.is_some() {Some(self.#option_field_names.clone().unwrap())} else {None}),*
                })
            }
        }
    );

    let generated_tokens = quote! {
        #builder_struct_def
        #builder_constructor
        #builder_impl
    };
    // this is a proc_macro2 TokenStream but it can be
    // converted into a "normal" TokenStream using into
    let generated_tokens = generated_tokens.into();
    // eprintln!("TOKENS: {}", generated_tokens);
    return generated_tokens;
}

fn parse_struct_fields(fields: syn::DataStruct) -> Vec<(Field, Option<Type>)> {
    // let struct_fields: Vec<(syn::Ident, syn::Type, bool)> =
    let struct_fields: Vec<(Field, Option<Type>)> =
        if let syn::Fields::Named(f) = fields.fields.clone() {
            f.named
                .iter()
                .map(|field| {
                    let ty: syn::Type = field.ty.clone();
                    if let syn::Type::Path(ref p) = ty {
                        let outer_type: Option<&syn::PathSegment> = p.path.segments.iter().next();
                        if outer_type.is_none() || outer_type.unwrap().ident != "Option" {
                            return (field.clone(), None);
                        }
                        let outer_type = outer_type.unwrap();
                        match &outer_type.arguments {
                            syn::PathArguments::None | syn::PathArguments::Parenthesized(_) => {
                                return (field.clone(), None)
                            }
                            syn::PathArguments::AngleBracketed(args) => {
                                // return Some(syn::PathArguments::AngleBracketed(args.clone()))
                                if let Some(GenericArgument::Type(t)) = args.args.first() {
                                    return (field.clone(), Some(t.clone()));
                                } else {
                                    return (field.clone(), None);
                                }
                            }
                        }
                    } else {
                        (field.clone(), None)
                    }
                })
                .collect()
        } else {
            Vec::new()
        };
    struct_fields
}
