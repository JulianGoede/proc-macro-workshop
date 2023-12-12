use proc_macro::TokenStream;
use proc_macro2;
use quote::quote;
use syn::{
    parse_macro_input, Data::Struct, DeriveInput, Fields,
};

fn parse_fields(data: syn::DataStruct) -> Vec<syn::Field> {
    if let Fields::Named(named_fields) = data.fields {
        return named_fields
            .named
            .iter()
            .map(|named_field| named_field.clone())
            .collect();
    }
    return Vec::new();
}

fn fields_to_idents(fields: Vec<syn::Field>) -> Vec<proc_macro2::Ident> {
    fields
        .iter()
        .filter(|f| f.ident.is_some())
        .map(|f| f.ident.clone().unwrap())
        .collect()
}

fn generate_field_setters(fields: Vec<syn::Field>) -> proc_macro2::TokenStream {
    let field_name_idents:  Vec<proc_macro2::Ident> = fields_to_idents(fields);
    let field_name_strings: Vec<String> = field_name_idents
        .iter()
        .map(|f| f.to_string())
        .collect();

    quote!(#(.field(#field_name_strings, &self.#field_name_idents))*)
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
                f.debug_struct(#struct_name)
                    #field_setters
                    .finish()
            }
        }
    );
    code
}

#[proc_macro_derive(CustomDebug)]
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
    use super::*;
    use proc_macro2::{Ident, Span};
    use syn::{token::Colon, DeriveInput};

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

        let actual_code = generate_field_setters(fields).to_string();
        let expected_code = quote!(
            .field("bar", &self.bar)
            .field("baz", &self.baz)
        )
        .to_string();
        assert_eq!(actual_code, expected_code);
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
}
