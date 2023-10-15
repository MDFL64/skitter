extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Persist)]
pub fn derive_answer_fn(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_ident = input.ident;
    let struct_generics = input.generics;

    let debug = false;
    
    let res = match input.data {
        syn::Data::Struct(data) => {
            match data.fields {
                syn::Fields::Named(fields) => {
                    let mut field_names = Vec::new();

                    for f in fields.named {
                        let ident = f.ident.unwrap();
                        field_names.push(ident);
                    }

                    quote! {
                        impl<'vm> crate::persist::Persist<'vm> for #struct_ident #struct_generics {
                            fn persist_write(&self, writer: &mut crate::persist::PersistWriter<'vm>) {
                                #(self.#field_names.persist_write(writer));*
                            }
                
                            fn persist_read(reader: &mut crate::persist::PersistReader<'vm>) -> Self {
                                Self {
                                    #(#field_names: crate::persist::Persist::persist_read(reader)),*
                                }
                            }
                        }
                    }
                }
                syn::Fields::Unnamed(fields) => {
                    let mut field_names = Vec::new();

                    for (i,_) in fields.unnamed.iter().enumerate() {
                        field_names.push(syn::Index::from(i));
                    }

                    quote! {
                        impl<'vm> crate::persist::Persist<'vm> for #struct_ident #struct_generics {
                            fn persist_write(&self, writer: &mut crate::persist::PersistWriter<'vm>) {
                                #(self.#field_names.persist_write(writer));*
                            }
                
                            fn persist_read(reader: &mut crate::persist::PersistReader<'vm>) -> Self {
                                Self {
                                    #(#field_names: crate::persist::Persist::persist_read(reader)),*
                                }
                            }
                        }
                    }
                }
                _ => panic!("persist unit struct?")
            }
        }
        syn::Data::Enum(data) => {
            let mut read_cases = Vec::new();
            let mut write_cases = Vec::new();

            for (variant_n,variant) in data.variants.iter().enumerate() {
                let variant_ident = &variant.ident;

                match variant.fields {
                    syn::Fields::Unit => {
                        read_cases.push(quote!{
                            #variant_n => Self::#variant_ident,
                        });

                        write_cases.push(quote!{
                            Self::#variant_ident => (#variant_n).persist_write(writer),
                        });
                    }
                    syn::Fields::Named(ref fields) => {
                        let mut field_names = Vec::new();

                        for f in fields.named.iter() {
                            let ident = f.ident.as_ref().unwrap();
                            field_names.push(ident);
                        }

                        read_cases.push(quote!{
                            #variant_n => Self::#variant_ident{
                                #(#field_names: crate::persist::Persist::persist_read(reader)),*
                            },
                        });

                        write_cases.push(quote!{
                            Self::#variant_ident{#(#field_names),*} => {
                                (#variant_n).persist_write(writer);
                                #(#field_names.persist_write(writer));*
                            },
                        });
                    }
                    syn::Fields::Unnamed(ref fields) => {
                        let mut field_names = Vec::new();
                        let mut field_tmp_names = Vec::new();

                        // yuck
                        let span = syn::__private::Span::call_site();

                        for (i,_) in fields.unnamed.iter().enumerate() {
                            field_names.push(syn::Index::from(i));
                            field_tmp_names.push(syn::Ident::new(&format!("f{}",i), span));
                        }

                        read_cases.push(quote!{
                            #variant_n => Self::#variant_ident{
                                #(#field_names: crate::persist::Persist::persist_read(reader)),*
                            },
                        });

                        write_cases.push(quote!{
                            Self::#variant_ident(#(#field_tmp_names),*) => {
                                (#variant_n).persist_write(writer);
                                #(#field_tmp_names.persist_write(writer));*
                            },
                        });
                    }
                }
            }

            let fail_msg = format!("bad variant {{}} for {}",struct_ident);

            quote! {
                impl<'vm> crate::persist::Persist<'vm> for #struct_ident #struct_generics {
                    fn persist_write(&self, writer: &mut crate::persist::PersistWriter<'vm>) {
                        match self {
                            #(#write_cases)*
                            _ => panic!("enum write")
                        }
                    }
        
                    fn persist_read(reader: &mut crate::persist::PersistReader<'vm>) -> Self {
                        let n = usize::persist_read(reader);
                        match n {
                            #(#read_cases)*
                            _ => panic!(#fail_msg,n)
                        }
                    }
                }
            }
        }
        _ => panic!("persist union?")
    };

    let res = TokenStream::from(res);

    if debug {
        println!("{}",res);
    }
    res
}
