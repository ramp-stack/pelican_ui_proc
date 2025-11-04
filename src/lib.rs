use syn::{parse_macro_input,  Data, DeriveInput, Fields, Field, Path, Type, TypePath};
use proc_macro2::{TokenStream, TokenTree, Literal};
use quote::quote;

enum ChildType {
    Vector(TokenTree),
    Child(TokenTree),
    HashMap(TokenTree),
    Opt(TokenTree),
    OptBox(TokenTree),
    Boxed(TokenTree),
    VecBox(TokenTree),
}

impl ChildType {
    fn from_field(ty: &Type, ident: TokenTree) -> Self {
        match ty {
            Type::Path(TypePath{path: Path{segments, ..}, ..}) if segments.first().filter(|s| s.ident.to_string() == "Option".to_string()).is_some() => {
                let optbox = if let syn::PathArguments::AngleBracketed(args) = &segments.first().unwrap().arguments {
                    if let syn::GenericArgument::Type(ty) = args.args.first().unwrap() {
                        if let Type::Path(TypePath{path: Path{segments, ..}, ..}) = ty {
                            segments.first().filter(|s| s.ident.to_string() == "Box".to_string()).is_some()
                        } else {false}
                    } else {false}
                } else {false};
                if optbox {ChildType::OptBox(ident)} else {ChildType::Opt(ident)}
            },
            Type::Path(TypePath{path: Path{segments, ..}, ..}) if segments.first().filter(|s| s.ident.to_string() == "Vec".to_string()).is_some() => {
                let vecbox = if let syn::PathArguments::AngleBracketed(args) = &segments.first().unwrap().arguments {
                    if let syn::GenericArgument::Type(ty) = args.args.first().unwrap() {
                        if let Type::Path(TypePath{path: Path{segments, ..}, ..}) = ty {
                            segments.first().filter(|s| s.ident.to_string() == "Box".to_string()).is_some()
                        } else {false}
                    } else {false}
                } else {false};
                if vecbox {ChildType::VecBox(ident)} else {ChildType::Vector(ident)}
            },
            Type::Path(TypePath{path: Path{segments, ..}, ..}) if segments.first().filter(|s| s.ident.to_string() == "HashMap" || s.ident.to_string() == "IndexMap").is_some() => {
                ChildType::HashMap(ident)
            },
            Type::Path(TypePath{path: Path{segments, ..}, ..}) if segments.first().filter(|s| s.ident.to_string() == "Box".to_string()).is_some() => {
                ChildType::Boxed(ident)
            },
            _ => ChildType::Child(ident)
        }
    }
}

#[proc_macro_derive(Component, attributes(skip))]
pub fn derive_component(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let has_tag = |field: &Field, tag: &str| field.attrs.iter().any(|attr| attr.meta.path().get_ident().map(|i| &i.to_string() == tag).unwrap_or_default());

    match input.data {
        Data::Struct(struc) => {
            let (layout, children): (TokenTree, Vec<ChildType>) = match struc.fields {
                Fields::Named(named) => {
                    let mut iterator = named.named.into_iter();
                    (
                        TokenTree::Ident(iterator.next().map(|f| f.ident).flatten().unwrap_or_else(|| {panic!("Component requires the first field of the structure to be the layout");})),
                        iterator.flat_map(|field|
                            (!has_tag(&field, "skip")).then(|| ChildType::from_field(&field.ty, TokenTree::Ident(field.ident.unwrap())))
                        ).collect()
                    )
                },
                Fields::Unnamed(unnamed) => {
                    let mut iterator = unnamed.unnamed.iter().enumerate();
                    iterator.next().unwrap_or_else(|| {panic!("Component requires the first field of the structure to be the layout");});
                    (
                        TokenTree::Literal(Literal::usize_unsuffixed(0)),
                        iterator.flat_map(|(index, field)|
                            (!has_tag(&field, "skip")).then(|| ChildType::from_field(&field.ty, TokenTree::Literal(Literal::usize_unsuffixed(index))))
                        ).collect()
                    )
                },
                Fields::Unit => {panic!("Component requires a Layout and at least one child")}
            };
            children.is_empty().then(|| {panic!("Component requires at least one child component in the structure");});

            let children_mut = TokenStream::from_iter(children.iter().map(|child| match child {
                ChildType::Vector(name) => quote!{children.extend(self.#name.iter_mut().map(|c| c as &mut dyn pelican_ui::drawable::Drawable));},
                ChildType::HashMap(name) => quote!{children.extend(self.#name.values_mut().map(|v| v as &mut dyn pelican_ui::drawable::Drawable));},
                ChildType::Child(name) => quote!{children.push(&mut self.#name as &mut dyn pelican_ui::drawable::Drawable);},
                ChildType::Opt(name) => quote!{if let Some(item) = self.#name.as_mut() {children.push(item as &mut dyn pelican_ui::drawable::Drawable);}},
                ChildType::OptBox(name) => quote!{if let Some(item) = self.#name.as_mut() {children.push(&mut **item as &mut dyn pelican_ui::drawable::Drawable);}},
                ChildType::Boxed(name) => quote!{children.push(&mut *self.#name as &mut dyn pelican_ui::drawable::Drawable);},
                ChildType::VecBox(name) => quote!{children.extend(self.#name.iter_mut().map(|c| &mut **c as &mut dyn pelican_ui::drawable::Drawable));}
            }));
            let children = TokenStream::from_iter(children.iter().map(|child| match child {
                ChildType::Vector(name) => quote!{children.extend(self.#name.iter().map(|c| c as &dyn pelican_ui::drawable::Drawable));},
                ChildType::HashMap(name) => quote!{children.extend(self.#name.values().map(|v| v as &dyn pelican_ui::drawable::Drawable));},
                ChildType::Child(name) => quote!{children.push(&self.#name as &dyn pelican_ui::drawable::Drawable);},
                ChildType::Opt(name) => quote!{if let Some(item) = self.#name.as_ref() {children.push(item as &dyn pelican_ui::drawable::Drawable);}},
                ChildType::OptBox(name) => quote!{if let Some(item) = self.#name.as_ref() {children.push(&**item as &dyn pelican_ui::drawable::Drawable);}},
                ChildType::Boxed(name) => quote!{children.push(&*self.#name as &dyn pelican_ui::drawable::Drawable);},
                ChildType::VecBox(name) => quote!{children.extend(self.#name.iter().map(|c| &**c as &dyn pelican_ui::drawable::Drawable));}
            }));

            proc_macro::TokenStream::from(quote!{
                impl #impl_generics Component for #name #ty_generics #where_clause {
                    fn children_mut(&mut self) -> Vec<&mut dyn pelican_ui::drawable::Drawable> {
                        let mut children = vec![];
                        #children_mut
                        children
                    }
                    fn children(&self) -> Vec<&dyn pelican_ui::drawable::Drawable> {
                        let mut children = vec![];
                        #children
                        children
                    }

                    fn request_size(&self, ctx: &mut Context, children: Vec<pelican_ui::layout::SizeRequest>) -> pelican_ui::layout::SizeRequest {
                        pelican_ui::layout::Layout::request_size(&self.#layout, ctx, children)
                    }
                    fn build(&mut self, ctx: &mut Context, size: (f32, f32), children: Vec<pelican_ui::layout::SizeRequest>) -> Vec<pelican_ui::layout::Area> {
                        pelican_ui::layout::Layout::build(&mut self.#layout, ctx, size, children)
                    }
                }
            })
        },
        Data::Enum(enu) => {
            let starts = enu.variants.into_iter().map(|v| {
                let name = v.ident;
                match v.fields {
                    Fields::Named(named) => {
                        let mut iterator = named.named.into_iter();
                        iterator.next().map(|f| (
                            if let Type::Path(TypePath{path: Path{segments, ..}, ..}) = f.ty {
                                segments.first().filter(|s| s.ident.to_string() == "Box".to_string()).is_some()
                            } else {false},
                            TokenStream::from(quote!{Self::#name{c, ..} => })
                        ))
                    },
                    Fields::Unnamed(unnamed) => {
                        let mut iterator = unnamed.unnamed.iter();
                        iterator.next().map(|f| (
                            if let Type::Path(TypePath{path: Path{segments, ..}, ..}) = &f.ty {
                                segments.first().filter(|s| s.ident.to_string() == "Box".to_string()).is_some()
                            } else {false},
                            TokenStream::from(quote!{Self::#name(c, ..) => })
                        ))
                    },
                    Fields::Unit => None,
                }.unwrap_or_else(|| {panic!("Enumerator Component requires the first field of each variant to be a Component");})
            }).collect::<Vec<_>>();
            let children_mut = TokenStream::from_iter(starts.iter().map(|(b, s)| if !b {quote!{#s vec![c as &mut dyn pelican_ui::drawable::Drawable],}} else {quote!{#s vec![&mut **c as &mut dyn pelican_ui::drawable::Drawable],}}));
            let children = TokenStream::from_iter(starts.iter().map(|(b, s)| if !b {quote!{#s vec![c as &dyn pelican_ui::drawable::Drawable],}} else {quote!{#s vec![&**c as &dyn pelican_ui::drawable::Drawable],}}));
            proc_macro::TokenStream::from(quote!{
                impl #impl_generics Component for #name #ty_generics #where_clause {
                    fn children_mut(&mut self) -> Vec<&mut dyn pelican_ui::drawable::Drawable> {
                        match self {
                            #children_mut
                        }
                    }
                    fn children(&self) -> Vec<&dyn pelican_ui::drawable::Drawable> {
                        match self {
                            #children
                        }
                    }

                    fn request_size(&self, ctx: &mut Context, mut children: Vec<pelican_ui::layout::SizeRequest>) -> pelican_ui::layout::SizeRequest {
                        children.remove(0)
                    }
                    fn build(&mut self, ctx: &mut Context, size: (f32, f32), children: Vec<pelican_ui::layout::SizeRequest>) -> Vec<pelican_ui::layout::Area> {
                        vec![pelican_ui::layout::Area{offset: (0, 0), size}]
                    }
                }
            })
        },
        Data::Union(_) => {panic!("Cannot implement Component for a Union")}
    }
}

#[proc_macro_derive(Plugin)]
pub fn derive_plugin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    proc_macro::TokenStream::from(quote!{
        impl #impl_generics Plugin for #name #ty_generics #where_clause {}
    })
}
