use proc_macro::TokenStream;
use quote::quote;

/// Generates a bunch of const "as" casts for qvek
#[proc_macro]
pub fn generate_conversions(_input: TokenStream) -> TokenStream {
    let types = [
        "u8", "u16", "u32", "u64", "u128", "usize", "i8", "i16", "i32", "i64", "i128", "isize",
        "f32", "f64",
    ];

    let mut output = proc_macro2::TokenStream::new();

    for src in &types {
        for dst in &types {
            let src_ty = syn::Ident::new(src, proc_macro2::Span::call_site());
            let dst_ty = syn::Ident::new(dst, proc_macro2::Span::call_site());

            output.extend(quote! {
                impl const ConvertFrom<#src_ty> for #dst_ty {
                    fn convert_from(src: #src_ty) -> #dst_ty {
                        src as #dst_ty
                    }
                }
            });
        }
    }

    TokenStream::from(output)
}
