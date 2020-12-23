use std::iter::FromIterator;
use proc_macro_error::{abort, ResultExt};
use quote::ToTokens;
use syn::{
    self, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    Attribute, Expr, ExprLit, Ident, Lit, LitBool, LitStr, Token,
};
use proc_macro2::TokenStream;

pub struct Input {
    pub attrs: Vec<Attribute>,
    pub rest: TokenStream,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let rest = input.parse()?;
        Ok(Input { attrs, rest })
    }
}

#[derive(Debug)]
pub struct Args(pub Punctuated<Arg, Token![,]>);

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Punctuated::<Arg, Token![,]>::parse_terminated(input)
            .map(Args)
    }
}

#[derive(Debug)]
pub enum Arg {
    Mermaid(Ident, LitStr),
    Placement(Ident, LitStr),
}

impl Parse for Arg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        use Arg::*;

        let name: Ident = input.parse()?;
        let name_str = name.to_string();

        if input.peek(Token![=]) {
            let assign_token = input.parse::<Token![=]>()?;
            if input.peek(LitStr) {
                let lit: LitStr = input.parse()?;
                match name_str.as_str() {
                    "mermaid" => Ok(Mermaid(name, lit)),
                    "placement" => Ok(Placement(name, lit)),
                    other => abort!(name, "unexpected attribute parameter `{}`, allowed parameters: [`mermaid`, `placement`]", name_str)
                }
            } else {
                abort!(assign_token, "expected `string literal` after `=`")
            }
        } else {
            abort!(name, "expected token `=` after the attribute parameter name")
        }
    }
}
