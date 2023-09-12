use proc_macro2::{TokenStream, Ident};
use quote::ToTokens;
use syn::{parse_quote, punctuated::Punctuated, Token};

extern crate proc_macro;

#[proc_macro_attribute]
pub fn profile(
  attr: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  eprintln!("attr: {attr}");
  eprintln!("input: {input}");
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
  let args: Punctuated<Ident, Token![,]> = {
    let mut args = vec![];
    for arg in func.sig.inputs {
      args.push(match arg {
        syn::FnArg::Receiver(_) => todo!(),
        syn::FnArg::Typed(typed) => match &*typed.pat {
          syn::Pat::Ident(ident) => ident.ident.clone(),
          _ => todo!(),
        },
      });
    }
    args.into_iter().collect()
  };
  outer_func.block.stmts = parse_quote!(
    func;
    let start_time = std::time::SystemTime::now();
    let start = std::time::Instant::now();
    let ret = #inner_ident(#args);
    let end = std::time::Instant::now();
    let end_time = std::time::SystemTime::now();
    profiler::GLOBAL_PROFILER::entry(#inner_name, end.duration_since(start), start_time, end_time, module_path!());
    return ret;
  );
  eprintln!("output: {}", outer_func.clone().into_token_stream());
  Ok(outer_func.into_token_stream())
}
