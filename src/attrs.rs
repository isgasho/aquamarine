use proc_macro2::{TokenStream, Ident, Span};
use proc_macro_error::{proc_macro_error, emit_call_site_warning, abort, ResultExt};
use itertools::{Itertools, Either};

use syn::{parse_macro_input, DeriveInput, AttributeArgs, Attribute, MetaNameValue, Lit, Ident};
use darling::FromMeta;
use quote::quote;
use std::iter;

pub enum Attr {
    /// Attribute that is to be forwarded as-is
    Forward(Attribute),
    /// Doc comment that cannot be forwarded as-is
    DocComment(Ident, String),
    /// Diagram start token
    DiagramStart(Ident),
    /// Diagram entry (line)
    DiagramEntry(Ident, String),
    /// Diagram end token
    DiagramEnd(Ident),
}

impl quote::ToTokens for Vec<Attr> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut attrs = self.iter().peekable();

        while attrs.peek().is_some() {
            let attr = attrs.next().unwrap();
            match attr {
                Attr::Forward(attr) => attr.to_tokens(tokens),
                Attr::DocComment(_, comment) => tokens.extend(quote! {
                    #[doc = #comment]
                }),
                Attr::DiagramStart(ident) => {
                    let diagram = attrs.take_while(|x| x != Attr::DiagramEnd)
                        .map(Attr::expect_diagram_entry_text);

                    let preabmle = iter::once(r#"<div class="mermaid">"#);
                    let postamble = iter::once("<div>");

                    let body = preabmle.chain(diagram).chain(postamble).join("\n");

                    tokens.extend(generate_diagram_rustdoc(&body));
                },
                // If that happens, then the parsing stage is faulty: doc comments outside of
                // in between Start and End tokens are to be emitted as Attr::Forward
                Attr::DiagramEntry(ident, body) => {
                    emit_call_site_warning!("encountered an unexpected attribute that's going to be ignored, this is a bug! ({})", body);
                },
                Attr::DiagramEnd(_) => (),
            }
        }
    }
}

fn generate_diagram_rustdoc(body: &str) -> TokenStream {
    quote! {
        #[doc = r#"<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>"#]
        #[doc = r#"<script>window.mermaid == null && mermaid.initialize({startOnLoad:true});</script>"#]
        #[doc = #body]
    }
}

pub fn convert_attrs(attrs: Vec<Attribute>) -> syn::Result<Vec<Attr>> {
    const START_TOKEN: &str = "```mermaid";
    const END_TOKEN: &str = "```";

    let mut is_inside_diagram_span = false;

    attrs.into_iter().flat_map(|attr| {
        match attr.path.as_ident().cloned() {
            Some(ident) if ident == "doc" => {
                // TODO: is it possible to have a #[doc] attrbute with no literal?
                let NameValue(MetaNameValue { lit: Str(s), .. }) = attr.parse_meta()
                    .expect("#[doc] attribute must always have a literal");

                let body = s.value();
                let (pre, start_token, body, end_token, post) = parse_attr_body(ident, body)

                if !is_inside_diagram_span {
                    if
                }

                Either::Left(parse_attr_body(ident, body))
            },
            _ => Either::Right(iter::once(Attr::Forward(attr)))
        }
    }).collect()
}

fn parse_attr_body(ident: Ident, body: String) -> (Option<&str>, Option<&str>, Option<&str>, Option<&str>, Option<&str>) {
    const ENTRY: &str = "```mermaid";
    const EXIT: &str = "```";

    let sp = body.find(ENTRY);
    let ep = match sp {
        Some(spos) => body[spos + ENTRY.len()..].find(EXIT).map(|p| p + spos),
        None => body.find(EXIT),
    };

    let nonempty = |x| !x.is_empty();

    let pre = Some(&body[..sp.unwrap_or(0)]).filter(nonempty);
    let start = sp.map(|pos| &body[pos..pos+ENTRY.len()]);
    let body = Some(&body[sp.unwrap_or(0)..ep.unwrap_or(body.len())]);
    let end = ep.map(|pos| &body[pos..pos+EXIT.len()]);
    let post = Some(&body[ep.unwrap_or(body.len())..]).filter(nonempty);

    (pre, start, body, end, post)
}

impl Attr {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Attr::Forward(attr) => attr.path.get_ident(),
            Attr::DocComment(ident, _) => Some(ident),
            Attr::DiagramStart(ident) => Some(ident),
            Attr::DiagramEntry(ident, _) => Some(ident),
            Attr::DiagramEnd(ident) => Some(ident),
        }
    }

    pub fn expect_diagram_entry_text(&self) -> &str {
        const ERR_MSG: &str = "unexpected attribute inside a diagram definition: only #[doc] is allowed";
        match self {
            Attr::DiagramEntry(_, body) => body.as_str(),
            _ => if let Some(ident) = x.as_ident() {
                abort!(ident, ERR_MSG)
            } else {
                panic!(ERR_MSG)
            }
        }
    }
}