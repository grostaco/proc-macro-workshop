use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Ident, Span, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Data, DeriveInput, Error,
    Fields, GenericArgument, PathArguments, Type, TypePath,
};

macro_rules! type_is_option {
    ($ty:expr) => {
        match $ty {
            Type::Path(TypePath { path, .. }) => path.segments.first().unwrap().ident == "Option",
            _ => false,
        }
    };
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;

    let fields = match input.data {
        Data::Struct(s) => match s.fields {
            Fields::Named(f) => f.named.into_iter().collect::<Vec<_>>(),
            _ => panic!("Unsupported"),
        },
        _ => panic!("Unsupported"),
    };

    let mut opt_fields = Vec::new();
    let mut nonopt_fields = Vec::new();
    let mut each_fields = Vec::new();

    for field in &fields {
        if type_is_option!(&field.ty) {
            opt_fields.push(field);
        } else if !field.attrs.is_empty() {
            each_fields.push(field);
        } else {
            nonopt_fields.push(field);
        }
    }

    let (f_nonopt_idents, f_nonopt_tys): (Vec<_>, Vec<_>) = nonopt_fields
        .iter()
        .map(|field| (&field.ident, &field.ty))
        .unzip();

    let (f_opt_idents, f_opt_tys): (Vec<_>, Vec<_>) = opt_fields
        .iter()
        .map(|field| (&field.ident, get_single_inner_angular(&field.ty).unwrap()))
        .unzip();

    let (f_each_idents, f_each_tys): (Vec<_>, Vec<_>) = each_fields
        .iter()
        .map(|field| (&field.ident, &field.ty))
        .unzip();

    let mut f_each_names = Vec::new();
    for field in &fields {
        for attr in &field.attrs {
            for tree in attr.tokens.clone() {
                match &tree {
                    TokenTree::Group(grp) if grp.delimiter() == Delimiter::Parenthesis => {
                        match &grp.stream().into_iter().take(3).collect::<Vec<_>>()[..] {
                            [TokenTree::Ident(ident), TokenTree::Punct(punct), TokenTree::Literal(literal)]
                                if ident == "each" && punct.as_char() == '=' =>
                            {
                                f_each_names.push(Ident::new(
                                    &literal.to_string().trim_matches('"'),
                                    Span::call_site(),
                                ));
                            }
                            [TokenTree::Ident(_), TokenTree::Punct(_), TokenTree::Literal(_)] => {
                                return Error::new(
                                    attr.path.span().join(attr.tokens.span()).unwrap(),
                                    "expected `builder(each = \"...\")`",
                                )
                                .into_compile_error()
                                .into();
                            }
                            _ => continue,
                        }
                    }
                    _ => continue,
                }
            }
        }
    }

    let f_inner_each_tys = f_each_tys
        .iter()
        .map(|ty| get_single_inner_angular(*ty).unwrap())
        .collect::<Vec<_>>();

    let builder_ident = Ident::new(&format!("{}Builder", &input.ident), Span::call_site());
    let tokens = quote! {
        use std::error::Error;
        use std::mem;

        pub struct #builder_ident {
            #(#f_nonopt_idents: std::option::Option<#f_nonopt_tys>,)*
            #(#f_opt_idents: std::option::Option<#f_opt_tys>,)*
            #(#f_each_idents: #f_each_tys,)*
        }
        impl #ident {
            pub fn builder() -> CommandBuilder {
                return CommandBuilder {
                    #(#f_nonopt_idents: None,)*
                    #(#f_opt_idents: None,)*
                    #(#f_each_idents: Vec::new(),)*
                }
            }
        }

        impl #builder_ident {
            #(fn #f_nonopt_idents(&mut self, #f_nonopt_idents: #f_nonopt_tys) -> &mut Self {
                self.#f_nonopt_idents = std::option::Option::Some(#f_nonopt_idents);
                self
            })*

            #(fn #f_opt_idents(&mut self, #f_opt_idents: #f_opt_tys) -> &mut Self {
                self.#f_opt_idents = std::option::Option::Some(#f_opt_idents);
                self
            })*

            #(fn #f_each_names(&mut self, #f_each_names: #f_inner_each_tys) -> &mut Self {
                self.#f_each_idents.push(#f_each_names);
                self
            })*

            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn Error>> {
                Ok(#ident {
                    #(#f_nonopt_idents: self.#f_nonopt_idents.take().ok_or(stringify!(#f_nonopt_idents is None, cannot build back into #ident))?,)*
                    #(#f_opt_idents: self.#f_opt_idents.take(),)*
                    #(#f_each_idents: mem::replace(&mut self.#f_each_idents, Vec::new()),)*
                })
            }
        }

    };
    tokens.into()
}

fn get_single_inner_angular(ty: &Type) -> Option<&Type> {
    match ty {
        syn::Type::Path(TypePath { path, .. }) => match &path.segments.first().unwrap().arguments {
            PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                match args.first().unwrap() {
                    GenericArgument::Type(ty) => Some(ty),
                    _ => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

// let (f_each_idents, (f_each_tys, f_each_names)): (Vec<_>, (Vec<_>, Vec<_>)) = each_fields.iter().map(|field| {
//     (
//         &field.ident,
//         (&field.ty,
//         field.attrs.iter().find_map(|attr| {
//             attr.tokens.clone().into_iter().find_map(|tree| match tree {
//                 TokenTree::Group(grp) if grp.delimiter() == Delimiter::Parenthesis => {
//                     match &grp.stream().into_iter().take(3).collect::<Vec<_>>()[..] {
//                         [TokenTree::Ident(ident), TokenTree::Punct(punct), TokenTree::Literal(literal)]
//                             if ident == "each" && punct.as_char() == '=' => {
//                                 Some(AttrResult::Ident(Ident::new(&literal.to_string().trim_matches('"'), Span::call_site())))
//                             }
//                         [TokenTree::Ident(ident), TokenTree::Punct(punct), TokenTree::Literal(literal)] =>
//                             Some(AttrResult::Error(Error::new(ident.span(), "Unexpected ident attribute"))),
//                         _ => None,
//                     }
//                 }
//                 _ => None,
//             })
//         }).unwrap(),
//         ))
// }).unzip();

// let (f_idents, (f_tys, f_attrs)): (Vec<_>, (Vec<_>, Vec<_>)) = match input.data {
//     Data::Struct(s) => match s.fields {
//         Fields::Named(f) => f
//             .named
//             .into_iter()
//             .map(|f| (f.ident.unwrap(), (f.ty, f.attrs)))
//             .unzip(),
//         _ => panic!("Unsupported"),
//     },
//     _ => panic!("Unsupported"),
// };

// // f_idents.iter().zip(f_tys.iter().zip(f_attrs.iter())).fold((Vec::new(), Vec::new()), |()|);

// let (f_nonopt_idents, f_nonopt_tys): (Vec<_>, Vec<_>) = f_idents
//     .iter()
//     .zip(f_tys.iter())
//     .filter(|(_, ty)| {
//         println!("{:#?}", ty);
//         match ty {
//             Type::Path(TypePath { path, .. }) => {
//                 path.segments.first().unwrap().ident != "Option"
//             }
//             _ => false,
//         }
//     })
//     .unzip();

// let (f_opt_idents, f_opt_tys): (Vec<_>, Vec<_>) = f_idents
//     .iter()
//     .zip(f_tys.iter())
//     .filter(|(_, ty)| match ty {
//         Type::Path(TypePath { path, .. }) => path.segments.first().unwrap().ident == "Option",
//         _ => false,
//     })
//     .map(|(ident, ty)| (ident, get_single_inner_angular(ty).unwrap().clone()))
//     .unzip();
