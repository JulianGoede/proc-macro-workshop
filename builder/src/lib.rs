extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data::Struct, DeriveInput, GenericArgument, PathSegment};

#[proc_macro_derive(Builder)]
pub fn derive(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    // println!("{:?}", input.clone());
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

    let field_names = if let syn::Fields::Named(f) = fields.fields.clone() {
        f.named
            .iter()
            .map(|field| field.ident.clone())
            .filter(|named_f| named_f.is_some())
            .collect()
    } else {
        Vec::new()
    };
    let outer_field_types = if let syn::Fields::Named(f) = fields.fields.clone() {
        f.named.iter().map(|field| field.ty.clone()).collect()
    } else {
        Vec::new()
    };

    // inner field type if type is optionized
    let inner_field_types = if let syn::Fields::Named(f) = fields.fields.clone() {
        f.named
            .iter()
            .map(|field| {
                let ty: syn::Type = field.ty.clone();
                if let syn::Type::Path(p) = ty {
                    let outer_type: Option<&syn::PathSegment> = p.path.segments.iter().next();
                    if outer_type.is_none() || outer_type.unwrap().ident != "Option" {
                        return None;
                    }
                    let outer_type = outer_type.unwrap();
                    match &outer_type.arguments {
                        syn::PathArguments::None | syn::PathArguments::Parenthesized(_) => return None,
                        syn::PathArguments::AngleBracketed(args) => {
                            // return Some(syn::PathArguments::AngleBracketed(args.clone()))
                            if let Some(GenericArgument::Type(t)) = args.args.first() {
                                return Some(t.clone());
                            } else {
                                return None;
                            }
                        }
                    }

                    // for path_segment in p.path.segments.iter().skip(1) {
                    //     let t: &syn::PathSegment = path_segment;
                    //     t.
                    // }
                    // return Some(syn::PathSegment::new());
                } else {
                    None
                }
            })
            .collect()
    } else {
        Vec::new()
    };

    let outer_type_is_option: Vec<bool> = inner_field_types.iter().map(|t| t.is_some()).collect();
    let outer_field_names: Vec<syn::Ident> =std::iter::zip(field_names.clone(), outer_type_is_option.clone()).filter(|(_, is_opt)| !is_opt.to_owned() ).map(|(t,_)| t.unwrap()).collect();
    let outer_field_types: Vec<syn::Type> =std::iter::zip(outer_field_types, outer_type_is_option.clone()).filter(|(_, is_opt)| !is_opt.to_owned() ).map(|(t,_)| t).collect();

    let non_opt_setters = quote!(
        #(
            pub fn #outer_field_names(&mut self, #outer_field_names: #outer_field_types) -> &mut Self {
                self.#outer_field_names = Some(#outer_field_names);
                self
            }
        )
        *
    );


    let inner_field_names: Vec<syn::Ident> =std::iter::zip(field_names.clone(), outer_type_is_option.clone()).filter(|(_, is_opt)| is_opt.to_owned() ).map(|(t,_)| t.unwrap()).collect();
    let inner_field_types: Vec<syn::Type> =std::iter::zip(inner_field_types, outer_type_is_option.clone()).filter(|(_, is_opt)| is_opt.to_owned() ).map(|(t,_)| t.unwrap()).collect();
    let opt_setters = quote!(
        #(
            pub fn #inner_field_names(&mut self, #inner_field_names: #inner_field_types) -> &mut Self {
                self.#inner_field_names = Some(#inner_field_names);
                self
            }
        )
        *
    );

    let generated_tokens = quote! {
        #vis struct #builder_name {
            #(#outer_field_names: Option<#outer_field_types>),*
            #(,#inner_field_names: Option<#inner_field_types>),*
        }

        impl #struct_name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#outer_field_names: None),*
                    #(,#inner_field_names: None),*
                }
            }
        }

        impl #builder_name {

            #non_opt_setters

            #opt_setters

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    if self.#outer_field_names.is_none() {
                        // return Err(format!("field `{}` must be set before calling .build", self.#field_names).into());
                        return Err("field must be set before calling .build".into());
                    }
                )*
                // self.#field_names.ok_or("field must be set before calling .build")?
                // Ok(#struct_name {
                //     #(#field_names: self.#field_names.ok_or("field must be set before calling .build")?),*
                // })
                Ok(#struct_name{
                    #(#outer_field_names: self.#outer_field_names.clone().unwrap()),*
                    #(,#inner_field_names: if self.#inner_field_names.is_some() {Some(self.#inner_field_names.clone().unwrap())} else {None}),*
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
