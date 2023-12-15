use proc_macro::TokenStream;
use proc_macro2::{self, Ident};
use quote::quote;
use syn::{parse_macro_input, Data::Struct, DeriveInput, Fields};

fn parse_fields(data: syn::DataStruct) -> Vec<syn::Field> {
    if let Fields::Named(named_fields) = data.fields {
        return named_fields
            .named
            .iter()
            .map(|named_field| named_field.clone())
            .collect();
    } else {
        Vec::new()
    }
}

fn get_debug_attr(meta: &syn::Meta) -> Option<syn::ExprLit> {
    match meta {
        syn::Meta::NameValue(meta_name_value) => {
            let ident = meta_name_value.path.get_ident();
            if ident.is_none() || ident.unwrap() != "debug" {
                return None;
            }
            if let syn::Expr::Lit(fmt) = &meta_name_value.value {
                Some(fmt).cloned()
            } else {
                None
            }
        }
        _ => None,
    }
}

fn get_fields_with_fmt(parsed_fields: Vec<syn::Field>) -> Vec<(syn::Ident, Option<String>)> {
    parsed_fields
        .iter()
        .map(|field| {
            let attr_fmt: Option<syn::LitStr> = field
                .attrs
                .iter()
                .map(|attr| get_debug_attr(&attr.meta))
                .filter_map(|fmt_maybe| {
                    if let Some(fmt) = fmt_maybe {
                        match fmt.lit {
                            syn::Lit::Str(s) => Some(s.clone()),
                            _ => None,
                        }
                    } else {
                        None
                    }
                })
                .collect::<Vec<syn::LitStr>>()
                .first()
                .cloned();
            (field.ident.clone(), attr_fmt)
        })
        .filter(|(ident, _)| ident.is_some())
        .map(|(ident, custom_fmt)| match custom_fmt {
            Some(fmt) => (ident.unwrap(), Some(fmt.value())),
            None => (ident.unwrap(), None),
        })
        .collect()
}

#[allow(dead_code)]
fn fields_to_idents(fields: Vec<syn::Field>) -> Vec<proc_macro2::Ident> {
    fields.iter().filter_map(|f| f.ident.clone()).collect()
}

fn generate_field_setters(fields: Vec<syn::Field>) -> Vec<proc_macro2::TokenStream> {
    let fields_with_custom_fmt: Vec<(Ident, Option<String>)> = get_fields_with_fmt(fields);
    let setters: Vec<proc_macro2::TokenStream> = fields_with_custom_fmt
        .iter()
        .enumerate()
        .map(|(i, (field, fmt))| {
            let field_name: String = field.to_string();
            let setter = match fmt {
                Some(custom_fmt) => {
                    quote!(
                        write!(f, " ")?;
                        write!(f, #field_name)?;
                        write!(f, ": ")?;
                        write!(f, "{}", format!(#custom_fmt, &self.#field))?;
                    )
                }
                None => {
                    quote!(
                        write!(f, " ")?;
                        write!(f, #field_name)?;
                        write!(f, ": \"")?;
                        write!(f, "{}", &self.#field)?;
                        write!(f, "\"")?;
                    )
                }
            };
            if i > 0 {
                quote!(
                    write!(f, ",")?;
                    #setter
                )
            } else {
                setter
            }
        })
        .collect();
    setters
}

fn generate_debug_impl(
    struct_ident: proc_macro2::Ident,
    fields: Vec<syn::Field>,
) -> proc_macro2::TokenStream {
    let field_setters = generate_field_setters(fields);
    let struct_name = struct_ident.to_string();
    let code = quote!(
        impl std::fmt::Debug for #struct_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, #struct_name)?;
                write!(f, " {{")?;
                #(#field_setters)*
                write!(f, " }}")
            }
        }
    );
    code
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_name = ast.ident;
    let data: syn::DataStruct = match ast.data {
        Struct(data_struct) => data_struct,
        _ => panic!("CustomDebug is only supported for structs"),
    };

    let named_fields = parse_fields(data);
    let code: proc_macro2::TokenStream = generate_debug_impl(struct_name, named_fields);
    code.into()
}

#[cfg(test)]
mod tests {
    use std::iter::zip;

    use super::*;
    use proc_macro2::{Delimiter, Ident, Span};
    use syn::{token::Colon, DeriveInput, Token};

    fn get_struct_tokens() -> proc_macro2::TokenStream {
        // fn get_struct_tokens() -> DeriveInput {
        let tokens = quote!(
            struct Foo {
                bar: String,
                baz: Integer,
            }
        );
        tokens
    }

    fn tokens_to_ast(tokens: proc_macro2::TokenStream) -> DeriveInput {
        let ast_result: Result<DeriveInput, syn::Error> = syn::parse2(tokens.clone());
        if let syn::Result::Ok(ast) = ast_result {
            return ast;
        } else {
            panic!("Couldn't convert to DeriveInput");
        }
    }

    fn ast_to_data_struct(ast: DeriveInput) -> syn::DataStruct {
        let data: syn::DataStruct = match ast.data {
            Struct(data_struct) => data_struct,
            _ => panic!("CustomDebug is only supported for structs"),
        };
        data
    }

    #[macro_export]
    macro_rules! create_field {
        ($field_name: expr, $field_ty: expr) => {
            syn::Field {
                attrs: Vec::new(),
                vis: syn::Visibility::Inherited,
                mutability: syn::FieldMutability::None,
                ident: Some(Ident::new($field_name, Span::call_site())),
                colon_token: Some(Colon {
                    spans: [Span::call_site(); 1],
                }),
                ty: syn::parse_str::<syn::Type>($field_ty).unwrap(),
            }
        };
    }

    #[test]
    fn test_parse_fields_yields_named_fields() {
        let input = get_struct_tokens();
        let ast = tokens_to_ast(input);
        let data = ast_to_data_struct(ast);
        let parsed_fields: Vec<syn::Field> = parse_fields(data);

        let expected_fields: Vec<syn::Field> = vec![
            create_field!("bar", "String"),
            create_field!("baz", "Integer"),
        ];
        assert_eq!(parsed_fields, expected_fields);
    }

    #[test]
    fn test_correct_field_setters_are_generated() {
        let fields: Vec<syn::Field> = vec![
            create_field!("bar", "String"),
            create_field!("baz", "Integer"),
        ];

        let actual_setters = generate_field_setters(fields);
        let expected_setters = vec![
            quote!(
                write!(f, " ")?;
                write!(f, "bar")?;
                write!(f, ": \"")?;
                write!(f, "{}", &self.bar)?;
                write!(f, "\"")?;
            ),
            quote!(
                write!(f, ",")?;
                write!(f, " ")?;
                write!(f, "baz")?;
                write!(f, ": \"")?;
                write!(f, "{}", &self.baz)?;
                write!(f, "\"")?;
            ),
        ];
        for (actual_setter, expected_setter) in zip(actual_setters, expected_setters) {
            assert_eq!(actual_setter.to_string(), expected_setter.to_string());
        }
    }

    #[test]
    fn test_fields_to_idents_returns_corresponding_idents() {
        let fields: Vec<syn::Field> = vec![
            create_field!("foo", "String"),
            create_field!("bar", "Integer"),
        ];

        let actual = fields_to_idents(fields);
        let expected = vec![
            Ident::new("foo", Span::call_site()),
            Ident::new("bar", Span::call_site()),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_field_attribute_is_parsed() {
        let debug_str = "0b{:08b}".to_string();
        let tokens = quote!(
            struct Foo {
                bar: String,
                #[debug = #debug_str]
                baz: Integer,
            }
        );
        let ast = tokens_to_ast(tokens);
        let data = ast_to_data_struct(ast);
        let parsed_fields: Vec<syn::Field> = parse_fields(data);

        let actual: Vec<(Ident, Option<String>)> = get_fields_with_fmt(parsed_fields);
        let expected = vec![
            (syn::Ident::new("bar", Span::call_site()), None),
            (syn::Ident::new("baz", Span::call_site()), Some(debug_str)),
        ];
        assert_eq!(actual, expected);
    }
}
