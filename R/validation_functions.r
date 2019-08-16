get_names = function(x) {
  nm = names(x)
  nm[nm != ""]
}
check_arg_name_overlap = function(lhs, manual,rhs_expr) {
  lhs_nms = get_names(lhs)
  manual_nms = get_names(manual)
  overlap = intersect(lhs_nms, manual_nms)
  if(length(overlap)>0) {
    rhs_name = rlang::expr_deparse(rhs_expr[[1]])
    overlap_args = paste(overlap, sep = "", collapse = ", ")
    msg = paste0("Some arguments piped to ",rhs_name,
                 " were also manually specified.\n",
                 "Piped values ignored for these arguments:\n", overlap_args)
    warning(msg, call. = FALSE)
  }
  invisible()
}
check_braces = function(rhs_expr) {
    !rlang::is_symbol(rhs_expr) &&
    identical(rhs_expr[[1]], rlang::expr(`{`))
}
check_paren = function(rhs_expr) {
  !rlang::is_symbol(rhs_expr) &&
  identical(rhs_expr[[1]], rlang::expr(`(`))
}
drop_braces = function(rhs_expr) {
  # remove parentheses/braces from rhs_expr if internal is valid
  if(length(rhs_expr) == 2) return(rhs_expr[[2]])
  else {
    type = ifelse(identical(rhs_expr[[1]], rlang::expr(`(`)),
                  "parentheses", "braces")
    stop("Invalid expression inside ", type, ":\n",
            rlang::expr_deparse(rhs_expr), call. = FALSE)
  }
}
is_function_def = function(rhs_expr) {
  rlang::is_formula(rhs_expr) || (
    rlang::is_call(rhs_expr) &&
      identical(rhs_expr[[1]], rlang::expr(`function`)))
}
validate_rhs = function(rhs_quo, rhs) {
  rhs_expr = rlang::get_expr(rhs_quo)
  eval_env = rlang::get_env(rhs_quo)
  # Make sure that anonymous functions are wrapped in braces
  if(is_function_def(rhs_expr)) {
    # If there's an unwrapped inline function definition,
    # the parser can't figure out what needs to happen
    type = rhs_expr[[1]]
    # browser()
    foo_exmpl = ifelse(identical(type, rlang::expr(`~`)),
                       "~.x + .y", "function(.x, .y) {.x + .y}")
    rlang::abort(paste0("Anonymous functions must be wrapped with () or {} to use the splice pipe.\n",
                        "good: list(.x = 1, .y = 2) %!>% {", foo_exmpl, "} \n",
                        "bad: list(.x = 1, .y = 2) %!>% ", foo_exmpl), )
  }
  # Next, remove braces
  while(check_braces(rhs_expr)) {
    rhs_expr = drop_braces(rhs_expr)
  }
  # First, check for parentheses; if they are present,
  # evaluate what's inside, define it as rhs, then
  if(check_paren(rhs_expr)) {
    rhs_expr = drop_braces(rhs_expr)
    # browser()
    if(rlang::is_call(rhs_expr[[1]])) {
      # If the first arg's a call, then it's the function factory
      tmp_quo = rlang::new_quosure(rhs_expr[[1]], env = eval_env)
      rhs_expr[[1]] = rlang::eval_tidy(tmp_quo)
    } else {
      # No arguments to the function (yet), so evaluate it and
      rhs = rlang::eval_tidy(rlang::as_function(rhs), env = eval_env)
      # browser()
      eval_env = environment(rhs)
      # Wrap it in a brace so it doesn't trip the braces removal
      # rhs_expr = rlang::call2(rlang::expr(`{`), rlang::enexpr(rhs))
      rhs_expr = rlang::call2(rhs)
    }
  }
  # browser()
  # Conversions
  if(rlang::is_symbol(rhs_expr)){
    # Assume this is a function
    if(rlang::is_function(rhs)) {
      rhs_expr = rlang::call2(rhs_expr)
    }
  } else if(is_function_def(rhs_expr)) {
    # convert formula into function call
    rhs_expr = rlang::call2(
      rlang::as_function(rhs, env = eval_env))
  } else{
    # rhs_expr = rhs_expr
  }
  # browser()
  rlang::new_quosure(expr = rhs_expr, env = eval_env)
}

