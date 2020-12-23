extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro_error::{proc_macro_error, abort, ResultExt};
use itertools::Itertools;

use syn::{parse_macro_input, DeriveInput, AttributeArgs, Attribute, MetaNameValue, Lit};
use darling::FromMeta;
use quote::quote;

mod parse;
mod attrs;

/// Example of user-defined [procedural macro attribute][1].
///
/// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#attribute-macros

#[proc_macro_attribute]
#[proc_macro_error]
pub fn aquamarine(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as parse::Args);
    let input = parse_macro_input!(input as parse::Input);

    println!("args = \n{:?}", args);

    check_attrs(&input.attrs);
    let attrs = process_attrs(input.attrs);
    let tokens = input.rest;
    let tokens = quote! {
        #(#attrs)*
        #tokens
    };

    tokens.into()
}

fn check_attrs(input: &[Attribute]) {
    for attr in input {
        // TODO: support multiple aquamarine entries
        if attr.path.is_ident("aquamarine") {
            abort!(attr, "multiple `aquamarine` attributes aren't supported -- use the doc comments instead");
        }
    }
}

fn process_attrs(attrs: Vec<Attribute>) -> Vec<proc_macro2::TokenStream> {
    use syn::Lit::*;
    use syn::Meta::*;

    const ENTRY: &str = "```mermaid";
    const EXIT: &str = "```";

    let mut output = Vec::with_capacity(attrs.len());
    let mut attrs = attrs.into_iter().peekable();

    let mut diagram = vec![];
    let mut is_diagram = false;

    for attr in attrs {
        if attr.path.is_ident("doc") {
            if let Ok(NameValue(MetaNameValue { lit: Str(s), .. })) = attr.parse_meta() {
                let s: String = s.value();

                dbg!(s.as_str());

                if !is_diagram {
                    if let Some(entry_pos) = s.find(ENTRY) {
                        is_diagram = true;
                    }
                } else {
                    if let Some(exit_pos) = s.find(EXIT) {
                        is_diagram = false;
                    } else {
                        diagram.push(s);
                    }
                }
            }
        }

        if !is_diagram {
            if diagram.is_empty() {
                output.push(quote! { #attr });
            } else {
                let doc_body = format!(r#"<div class = "mermaid">{}</div>"#, diagram.drain(..).join("\n"));
                output.push(quote! {
                    #[doc = r#"<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>"#]
                    #[doc = r#"<script>window.mermaid == null && mermaid.initialize({startOnLoad:true});</script>"#]
                    #[doc = #doc_body]
                });
            }
        }
    }

    output
}