use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse_quote;

extern crate proc_macro;

#[proc_macro_attribute]
pub fn profile(
  attr: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  match profile_inner(input, attr.into()) {
    Ok(res) => res.into(),
    Err(err) => err.into_compile_error().into(),
  }
}

fn profile_inner(input: proc_macro::TokenStream, _attr: TokenStream) -> syn::Result<TokenStream> {
  let mut func: syn::ItemFn = syn::parse(input)?;
  let inner_name = format!("inner_{}", func.sig.ident);
  let inner_ident = syn::Ident::new(&inner_name, func.sig.ident.span());
  let inner_block = func.block.clone();
  let return_type = func.sig.output.clone();
  let inner_closure: syn::ExprClosure = parse_quote! {
    move || #return_type {
      return #inner_block;
    }
  };
  func.block.stmts = parse_quote!(
    let mut #inner_ident = #inner_closure;
    let start_time = std::time::SystemTime::now();
    let start = std::time::Instant::now();
    let ret = #inner_ident();
    let end = std::time::Instant::now();
    let end_time = std::time::SystemTime::now();
    profiler::GLOBAL_PROFILER.entry(#inner_name, end.duration_since(start), start_time, end_time, module_path!());
    return ret;
  );
  // eprintln!("output: {}", func.clone().into_token_stream());
  Ok(func.into_token_stream())
}

// use proc_macro2::{Ident, Span, TokenStream};
// use quote::ToTokens;
// use syn::{parse_quote, punctuated::Punctuated, spanned::Spanned, Token};

// extern crate proc_macro;

// #[proc_macro_attribute]
// pub fn profile(
//   attr: proc_macro::TokenStream,
//   input: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//   eprintln!("attr: {attr}");
//   eprintln!("input: {input}");
//   match profile_inner(input, attr.into()) {
//     Ok(res) => res.into(),
//     Err(err) => err.into_compile_error().into(),
//   }
// }

// fn profile_inner(input: proc_macro::TokenStream, _attr: TokenStream) -> syn::Result<TokenStream> {
//   let mut func: syn::ItemFn = syn::parse(input)?;
//   let mut outer_func = func.clone();
//   let inner_name = format!("inner_{}", func.sig.ident);
//   let inner_ident = syn::Ident::new(&inner_name, func.sig.ident.span());
//   func.sig.ident = inner_ident.clone();
//   let mut outer_receiver = None;
//   let args: Punctuated<Ident, Token![,]> = {
//     let mut args = vec![];
//     for arg in func.sig.inputs.iter() {
//       args.push(match arg {
//         syn::FnArg::Receiver(receiver) => {
//           let self_ty_ident = Ident::new("Self", receiver.span());
//           let this_ident = Ident::new("this", receiver.span());
//           outer_receiver = Some((receiver.clone(), this_ident.clone(), self_ty_ident));
//           this_ident
//         }
//         syn::FnArg::Typed(typed) => match &*typed.pat {
//           syn::Pat::Ident(ident) => ident.ident.clone(),
//           _ => todo!(),
//         },
//       });
//     }
//     args.into_iter().collect()
//   };
//   if let Some((rx, this_ident, self_ty_ident)) = outer_receiver {
//     let ty = Box::new({
//       let mut segments = Punctuated::new();
//       segments.push(syn::PathSegment {
//         ident: self_ty_ident,
//         arguments: syn::PathArguments::None,
//       });
//       let ty = syn::Type::Path(syn::TypePath {
//         qself: None,
//         path: syn::Path {
//           leading_colon: None,
//           segments,
//         },
//       });
//       if let Some((and_token, lifetime)) = rx.reference {
//         syn::Type::Reference(syn::TypeReference {
//           and_token,
//           lifetime,
//           mutability: rx.mutability,
//           elem: Box::new(ty),
//         })
//       } else {
//         ty
//       }
//     });
//     replace_self_in_block(&mut func.block, this_ident.clone());
//     *func.sig.inputs.first_mut().unwrap() = syn::FnArg::Typed(syn::PatType {
//       attrs: vec![],
//       pat: Box::new(syn::Pat::Ident(syn::PatIdent {
//         attrs: rx.attrs,
//         by_ref: None,
//         mutability: None,
//         ident: this_ident,
//         subpat: None,
//       })),
//       colon_token: rx.colon_token.unwrap_or(Token![:](Span::call_site())),
//       ty,
//     })
//   }
//   outer_func.block.stmts = parse_quote!(
//     #func;
//     let start_time = std::time::SystemTime::now();
//     let start = std::time::Instant::now();
//     let ret = #inner_ident(#args);
//     let end = std::time::Instant::now();
//     let end_time = std::time::SystemTime::now();
//     profiler::GLOBAL_PROFILER.entry(#inner_name, end.duration_since(start), start_time, end_time, module_path!());
//     return ret;
//   );
//   eprintln!("output: {}", outer_func.clone().into_token_stream());
//   Ok(outer_func.into_token_stream())
// }

// fn replace_self_in_block(block: &mut syn::Block, this_ident: Ident) {
//   for stmt in block.stmts.iter_mut() {
//     match stmt {
//       syn::Stmt::Local(syn::Local {
//         init: Some(syn::LocalInit { expr, diverge, .. }),
//         ..
//       }) => {
//         replace_self(expr, this_ident.clone());
//         if let Some((_, diverge)) = diverge {
//           replace_self(diverge, this_ident.clone());
//         }
//       }
//       syn::Stmt::Expr(expr, _) => replace_self(expr, this_ident.clone()),
//       syn::Stmt::Item(_) => continue,
//       syn::Stmt::Macro(_) => continue,
//       _ => continue,
//     }
//   }
// }

// fn replace_self(expr: &mut syn::Expr, this_ident: Ident) {
//   match expr {
//     syn::Expr::Array(array) => array
//       .elems
//       .iter_mut()
//       .for_each(|e| replace_self(e, this_ident.clone())),
//     syn::Expr::Assign(assign) => {
//       replace_self(&mut assign.left, this_ident.clone());
//       replace_self(&mut assign.right, this_ident.clone());
//     }
//     syn::Expr::Async(async_block) => {
//       replace_self_in_block(&mut async_block.block, this_ident.clone());
//     }
//     syn::Expr::Await(await_expr) => {
//       replace_self(&mut await_expr.base, this_ident.clone());
//     }
//     syn::Expr::Binary(binary) => {
//       replace_self(&mut binary.left, this_ident.clone());
//       replace_self(&mut binary.right, this_ident.clone());
//     }
//     syn::Expr::Block(block) => {
//       replace_self_in_block(&mut block.block, this_ident.clone());
//     }
//     syn::Expr::Break(break_expr) => {
//       if let Some(break_value) = break_expr.expr.as_mut() {
//         replace_self(break_value, this_ident)
//       }
//     }
//     syn::Expr::Call(call) => {
//       call
//         .args
//         .iter_mut()
//         .for_each(|e| replace_self(e, this_ident.clone()));
//       replace_self(&mut call.func, this_ident.clone());
//     }
//     syn::Expr::Cast(cast) => replace_self(&mut cast.expr, this_ident.clone()),
//     syn::Expr::Closure(closure) => {
//       replace_self(&mut closure.body, this_ident.clone());
//     },
//     syn::Expr::Const(_) => return,
//     syn::Expr::Continue(_) => return,
//     syn::Expr::Field(field) => replace_self(field.base.as_mut(), this_ident.clone()),
//     syn::Expr::ForLoop(for_loop) => {
//       replace_self(&mut for_loop.expr, this_ident.clone());
//       replace_self_in_block(&mut for_loop.body, this_ident.clone());
//     },
//     syn::Expr::Group(group) => replace_self(&mut group.expr, this_ident.clone()),
//     syn::Expr::If(if_expr) => {
//       replace_self(&mut if_expr.cond, this_ident.clone());
//       replace_self_in_block(&mut if_expr.then_branch, this_ident.clone());
//       if let Some((_, else_br)) = &mut if_expr.else_branch {
//         replace_self(else_br, this_ident.clone());
//       }
//     },
//     syn::Expr::Index(index) => {
//       replace_self(&mut index.expr, this_ident.clone());
//       replace_self(&mut index.index, this_ident.clone());
//     },
//     syn::Expr::Infer(_) => return,
//     syn::Expr::Let(let_expr) => replace_self(&mut let_expr.expr, this_ident.clone()),
//     syn::Expr::Lit(_) => return,
//     syn::Expr::Loop(loop_expr) => replace_self_in_block(&mut loop_expr.body, this_ident.clone()),
//     syn::Expr::Macro(_) => todo!("macro"),
//     syn::Expr::Match(match_expr) => {
//       replace_self(&mut match_expr.expr, this_ident.clone());
//       for arm in match_expr.arms.iter_mut() {
//         replace_self(&mut arm.body, this_ident.clone());
//         if let Some((_, guard)) = &mut arm.guard {
//           replace_self(guard, this_ident.clone());
//         }
//       }
//     },
//     syn::Expr::MethodCall(method) => {
//       replace_self(&mut method.receiver, this_ident.clone());
//       for arg in method.args.iter_mut() {
//         replace_self(arg, this_ident.clone());
//       }
//     },
//     syn::Expr::Paren(paren) => replace_self(&mut paren.expr, this_ident),
//     syn::Expr::Path(path) => {
//       if path.path.is_ident("self") {
//         path.path.segments.clear();
//         path.path.segments.push(syn::PathSegment{ident: this_ident, arguments: syn::PathArguments::None});
//       }
//     },
//     syn::Expr::Range(range) => {
//       if let Some(start) = &mut range.start {
//         replace_self(start, this_ident.clone());
//       }
//       if let Some(end) = &mut range.end {
//         replace_self(end, this_ident.clone());
//       }
//     },
//     syn::Expr::Reference(reference) => replace_self(&mut reference.expr, this_ident),
//     syn::Expr::Repeat(repeat) => {
//       replace_self(&mut repeat.expr, this_ident.clone());
//       replace_self(&mut repeat.len, this_ident);
//     },
//     syn::Expr::Return(return_expr) => {
//       if let Some(return_val) = &mut return_expr.expr {
//         replace_self(return_val, this_ident);
//       }
//     },
//     syn::Expr::Struct(struct_expr) => {
//       for field in struct_expr.fields.iter_mut() {
//         replace_self(&mut field.expr, this_ident.clone());
//       }
//       if let Some(rest) = &mut struct_expr.rest {
//         replace_self(rest, this_ident);
//       }
//     },
//     syn::Expr::Try(try_expr) => replace_self(&mut try_expr.expr, this_ident),
//     syn::Expr::TryBlock(try_block) => replace_self_in_block(&mut try_block.block, this_ident),
//     syn::Expr::Tuple(tuple) => {
//       for val in tuple.elems.iter_mut() {
//         replace_self(val, this_ident.clone());
//       }
//     },
//     syn::Expr::Unary(unary) => replace_self(&mut unary.expr, this_ident),
//     syn::Expr::Unsafe(unsafe_expr) => replace_self_in_block(&mut unsafe_expr.block, this_ident),
//     syn::Expr::Verbatim(_) => todo!("Verbatim"),
//     syn::Expr::While(while_expr) => {
//       replace_self(&mut while_expr.cond, this_ident.clone());
//       replace_self_in_block(&mut while_expr.body, this_ident);
//     },
//     syn::Expr::Yield(yield_expr) => if let Some(yield_val) = &mut yield_expr.expr {
//       replace_self(yield_val, this_ident);
//     },
//     _ => todo!(),
//   }
// }
