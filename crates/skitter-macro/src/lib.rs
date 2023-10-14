extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Persist)]
pub fn derive_answer_fn(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_ident = input.ident;
    let struct_generics = input.generics;

    let mut debug = false;
    
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
                        impl<'vm> Persist<'vm> for #struct_ident #struct_generics {
                            fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
                                #(self.#field_names.persist_write(writer));*
                            }
                
                            fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
                                Self {
                                    #(#field_names: Persist::persist_read(reader)),*
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
                        impl<'vm> Persist<'vm> for #struct_ident #struct_generics {
                            fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
                                #(self.#field_names.persist_write(writer));*
                            }
                
                            fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
                                Self {
                                    #(#field_names: Persist::persist_read(reader)),*
                                }
                            }
                        }
                    }
                }
                _ => panic!("todo weird struct")
            }
        }
        _ => panic!("todo enum?")
    };

    let res = TokenStream::from(res);

    if debug {
        println!("{}",res);
    }
    res
}
