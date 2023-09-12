use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse_quote;

extern crate proc_macro;

#[proc_macro_attribute]
pub fn profile(
  input: proc_macro::TokenStream,
  attr: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  match profile_inner(input, attr.into()) {
    Ok(res) => res.into(),
    Err(err) => err.into_compile_error().into(),
  }
}

fn profile_inner(input: proc_macro::TokenStream, _attr: TokenStream) -> syn::Result<TokenStream> {
  let mut func: syn::ItemFn = syn::parse(input)?;
  let mut outer_func = func.clone();
  let inner_name = format!("inner_{}", func.sig.ident);
  let inner_ident = syn::Ident::new(&inner_name, func.sig.ident.span());
  func.sig.ident = inner_ident.clone();
  outer_func.block.stmts = parse_quote!(
    func
    let start_time = std::time::SystemTime::now();
    let start = std::time::Intent::now();
    let ret = #inner_ident();
    let end = std::time::Intent::now();
    let end_time = std::time::SystemTime::now();
    profiler::GLOBAL_PROFILER::entry(#inner_name, end.duration_since(start), start_time, end_time, module_path!());
    ret
  );
  Ok(outer_func.into_token_stream())
}
