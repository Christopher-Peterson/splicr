named_elements = function(x) {
  nm = names(x)
  if(is.null(nm)) return(character(0))
  nm[nm != ""]
}

arg_overlap_condition = function(msg, ...) {
  cond = getOption("splicr_arg_overlap_cond", default = "warning")
  cond = match.arg(cond,
       c("warning", "error", "message", "silent"))
  cond_fun = list(warning = rlang::warn, error = rlang::abort,
                  message = rlang::inform,
                  silent = rlang::signal)[[cond]]
  cond_fun(msg, ...)
}

#' Change how multi-pipe (\%!>\%) handles argument conflicts
#'
#' By default, attempting to use the same argument on the right
#' left sides of a multi-pipe (e.g., \code{list(a = 1) \%!>\% fun(a = 2)} )
#' produces a warning, and the right-hand side argument is used.
#' This sets a global option that changes the warning into
#' an error, message, or silent condition.
#'
#' @param cond The condition triggered by conflicting arguments
#' @export
#' @seealso \%!>\%
set_splicr_conflict_handling =
  function(cond = c("warning", "error", "message", "silent")){
    cond = match.arg(cond)
    options(splicr_arg_overlap_cond = cond)
    invisible(cond)
  }

check_arg_name_overlap = function(lhs, manual,rhs_expr) {
  lhs_nms = named_elements(lhs)
  manual_nms = named_elements(manual)
  overlap = intersect(lhs_nms, manual_nms)
  if(length(overlap)>0) {
    rhs_name = expr_deparse(rhs_expr[[1]])
    overlap_args = paste(overlap, sep = "", collapse = ", ")
    msg = paste0("Some arguments piped to ",rhs_name,
                 " were also manually specified.\n",
                 "Piped values ignored for these arguments:\n",
                 overlap_args, "\n")
    arg_overlap_condition(msg)
  }
  invisible()
}
check_braces = function(rhs_expr) {
    !is_symbol(rhs_expr) &&
    identical(rhs_expr[[1]], expr(`{`))
}
check_paren = function(rhs_expr) {
  !is_symbol(rhs_expr) &&
  identical(rhs_expr[[1]], expr(`(`))
}
drop_braces = function(rhs_expr) {
  # remove parentheses/braces from rhs_expr if internal is valid
  if(length(rhs_expr) == 2) return(rhs_expr[[2]])
  else {
    type = ifelse(identical(rhs_expr[[1]], expr(`(`)),
                  "parentheses", "braces")
    stop("Invalid expression inside ", type, ":\n",
            expr_deparse(rhs_expr), call. = FALSE)
  }
}
is_function_def = function(rhs_expr) {
  rlang::is_formula(rhs_expr) || (
    rlang::is_call(rhs_expr) &&
      identical(rhs_expr[[1]], expr(`function`)))
}
assert_lambdas_in_braces = function(rhs_expr) {
  if(is_function_def(rhs_expr)) {
    # If there's an unwrapped inline function definition,
    # the parser can't figure out what needs to happen
    type = rhs_expr[[1]]
    # browser()
    foo_exmpl = ifelse(identical(type, expr(`~`)),
                       "~.x + .y", "function(.x, .y) {.x + .y}")
    rlang::abort(paste0("Anonymous functions must be wrapped with () or {} to use the splice pipe.\n",
                        "good: list(.x = 1, .y = 2) %!>% {", foo_exmpl, "} \n",
                        "bad: list(.x = 1, .y = 2) %!>% ", foo_exmpl), )
  }
  invisible()
}
eval_paren = function(rhs_quo, rhs) {
# Need to look into if I can avoid rhs quo here or rhs
  rhs_expr = drop_braces(get_expr(rhs_quo))
  eval_env = get_env(rhs_quo)
  if(!rlang::is_symbol(rhs_expr) &&
    rlang::is_call(rhs_expr[[1]])) {
    # If the first arg's a call, then it's the function factory
    tmp_quo = new_quosure(rhs_expr[[1]], env = eval_env)
    rhs_expr[[1]] = eval_tidy(tmp_quo)
  } else {
    if(is_symbol(rhs_expr)) {
      # add parens to function
      rhs = rlang::exec(rhs, .env = eval_env)
    } else {
      # No arguments to the function (yet), so evaluate it and
      rhs = eval_tidy(rlang::as_function(rhs), env = eval_env)
    }
    eval_env = environment(rhs)
    rhs_expr = call2(rhs)
  }
  new_quosure(rhs_expr, eval_env)
}
validate_rhs = function(rhs_quo, rhs) {
  rhs_expr = get_expr(rhs_quo)
  eval_env = get_env(rhs_quo)
  # Make sure that anonymous functions are wrapped in braces
  assert_lambdas_in_braces(rhs_expr)
  # Next, remove braces
  while(check_braces(rhs_expr)) {
    rhs_expr = drop_braces(rhs_expr)
  }
  rhs_quo = new_quosure(rhs_expr, eval_env)
  if(check_paren(rhs_expr)) {
    # First, check for parentheses; if they are present,
    # evaluate what's inside, define it as rhs, then
    rhs_quo = eval_paren(rhs_quo, rhs)
    rhs_expr = get_expr(rhs_quo)
  }
  # Conversions
  eval_env = get_env(rhs_quo)
  if(is_symbol(rhs_expr)){
    # Assume this is a function
    if(rlang::is_function(rhs)) {
      rhs_quo = new_quosure(
        call2(rhs_expr), eval_env)
    }
  } else if(is_function_def(rhs_expr)) {
    # convert formula into function call
    rhs_quo = new_quosure(
      call2(rlang::as_function(
        rhs, env = eval_env)), eval_env)
  }
  rhs_quo
}

