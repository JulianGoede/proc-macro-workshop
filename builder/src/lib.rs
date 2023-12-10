extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data::Struct, DeriveInput, Error, Field, GenericArgument, Ident, Type, spanned::Spanned,
};

enum FieldType {
    Boring,
    Option(Type),
    Vec(Type),
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    // eprintln!("INPUT: {:#?}", input);
    let struct_name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), struct_name.span());
    let vis = syn::Visibility::Inherited;
    let fields: syn::DataStruct = match input.data {
        Struct(fields) => fields,
        other => {
            panic!("expected syn::Data::Struct but got {:?}", other)
        }
    };

    let struct_fields = parse_struct_fields(fields);

    for (field, ..) in struct_fields.iter() {
        if let Err(e) = get_repeat_token(field) {
            return Error::into_compile_error(e).into();
        }
    }

    let (requried_field_names, requried_field_types): (Vec<Ident>, Vec<Type>) = struct_fields
        .iter()
        .filter(|(field, ..)| get_repeat_token(field).unwrap().is_none())
        .filter(|(.., option_type)| match option_type {
            FieldType::Boring => true,
            FieldType::Vec(_) => true,
            _ => false,
        })
        .map(|(field, __)| (field.ident.clone().unwrap(), field.ty.clone()))
        .unzip();
    let (option_field_names, option_field_types): (Vec<Ident>, Vec<Type>) = struct_fields
        .iter()
        .filter(|(field, ..)| get_repeat_token(field).unwrap().is_none())
        .filter(|(.., option_type)| match option_type {
            FieldType::Option(_) => true,
            _ => false,
        })
        .map(|(field, option_type)| {
            if let FieldType::Option(inner_type) = option_type {
                return (field.ident.clone().unwrap(), inner_type.clone());
            }
            // this should actually be unreachable
            return (field.ident.clone().unwrap(), field.ty.clone());
        })
        .unzip();

    let ((repeated_field_names, repeated_field_types), single_name): (
        (Vec<Ident>, Vec<Type>),
        Vec<Ident>,
    ) = struct_fields
        .iter()
        .filter(|(field, ..)| get_repeat_token(field).unwrap().is_some())
        .map(|(field, field_type)| {
            let token = get_repeat_token(field).unwrap().unwrap();
            let field: (Ident, Type) = if let FieldType::Vec(inner_type) = field_type {
                (field.ident.clone().unwrap(), inner_type.clone())
            } else {
                (field.ident.clone().unwrap(), field.ty.clone())
            };
            (field, token)
        })
        .unzip();

    let all_at_once_setter: Vec<proc_macro2::TokenStream> = repeated_field_names
        .iter()
        .zip(repeated_field_types.iter())
        .zip(single_name.iter())
        .filter(|((field_name, _), item_name)| field_name.to_string() != item_name.to_string())
        .map(|((field_name, ty), _)| {
            quote!(
                pub fn #field_name(&mut self, #field_name: Vec<#ty>) -> &mut Self {
                    self.#field_name = #field_name;
                    self
                }
            )
            .into()
        })
        .collect();

    let setters = quote!(
        #(
            pub fn #requried_field_names(&mut self, #requried_field_names: #requried_field_types) -> &mut Self {
                self.#requried_field_names = Some(#requried_field_names);
                self
            }
        )
        *
        #(#all_at_once_setter)
        *
        #(
            pub fn #option_field_names(&mut self, #option_field_names: #option_field_types) -> &mut Self {
                self.#option_field_names = Some(#option_field_names);
                self
            }
        )
        *
        #(
            pub fn #single_name(&mut self, #single_name: #repeated_field_types) -> &mut Self {
                self.#repeated_field_names.push(#single_name);
                self
            }
        )
        *
    );

    let builder_struct_def = quote!(
        #vis struct #builder_name {
            #(#requried_field_names: Option<#requried_field_types>,)*
            #(#repeated_field_names: Vec<#repeated_field_types>,)*
            #(#option_field_names: Option<#option_field_types>,)*
        }
    );

    let builder_constructor = quote!(
        impl #struct_name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#requried_field_names: None,)*
                    #(#repeated_field_names: Vec::<#repeated_field_types>::new(),)*
                    #(#option_field_names: None,)*
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
                    #(#requried_field_names: self.#requried_field_names.clone().unwrap(),)*
                    #(#repeated_field_names: self.#repeated_field_names.clone(),)*
                    #(#option_field_names: if self.#option_field_names.is_some() {Some(self.#option_field_names.clone().unwrap())} else {None},)*
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

fn parse_struct_fields(fields: syn::DataStruct) -> Vec<(Field, FieldType)> {
    let struct_fields: Vec<(Field, FieldType)> =
        if let syn::Fields::Named(f) = fields.fields.clone() {
            f.named
                .iter()
                .map(|field| {
                    let ty: Type = field.ty.clone();
                    if let Type::Path(ref p) = ty {
                        let outer_type: Option<&syn::PathSegment> = p.path.segments.iter().next();
                        if outer_type.is_none()
                            || (outer_type.unwrap().ident != "Option"
                                && outer_type.unwrap().ident != "Vec")
                        {
                            return (field.clone(), FieldType::Boring);
                        }
                        let outer_type = outer_type.unwrap();
                        match &outer_type.arguments {
                            syn::PathArguments::None | syn::PathArguments::Parenthesized(_) => {
                                return (field.clone(), FieldType::Boring)
                            }
                            syn::PathArguments::AngleBracketed(args) => {
                                // return Some(syn::PathArguments::AngleBracketed(args.clone()))
                                if let Some(GenericArgument::Type(t)) = args.args.first() {
                                    if outer_type.ident == "Vec" {
                                        return (field.clone(), FieldType::Vec(t.clone()));
                                    }
                                    return (field.clone(), FieldType::Option(t.clone()));
                                } else {
                                    return (field.clone(), FieldType::Boring);
                                }
                            }
                        }
                    } else {
                        (field.clone(), FieldType::Boring)
                    }
                })
                .collect()
        } else {
            Vec::new()
        };
    struct_fields
}

// fn join_spans(span_1: proc_macro2::Span, span_2: proc_macro2::Span) -> proc_macro2::Span {
//     let start_1 = span_1.start();
//     let end_1 = span_1.end();
//     let start_2 = span_2.start();
//     let end_2 = span_2.end();
//
//     let min_start = if start_1 < start_2 { start_1 } else { start_2 };
//     let max_end = if end_1 > end_2 { end_1 } else { end_2 };
//
//     proc_macro2::Span::new(min_start.line, min_start.column, max_end.line, max_end.column, None)
// }


fn get_repeat_token(field: &Field) -> syn::Result<Option<Ident>> {
    for attr in &field.attrs {
        if let syn::Meta::List(meta_list) = &attr.meta {
            let mut tokens = meta_list.tokens.clone().into_iter();
            if let Some(first_token) = tokens.next() {
                if first_token.to_string() == "each" {
                    if let Some(last_token) = tokens.last() {
                        let name: String = last_token.to_string();
                        if name.len() <= 2 {
                            return Err(syn::Error::new(
                                first_token.span(),
                                "field name was not provided",
                            ));
                        }
                        return Ok(Some(Ident::new(
                            &name[1..name.len() - 1],
                            last_token.span(),
                        )));
                    }
                } else {
                    return Err(syn::Error::new_spanned(attr.meta.clone(), "expected `builder(each = \"...\")`"));
                }
            }
        }
    }
    return Ok(None);
}
