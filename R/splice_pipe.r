#' Splice pipe operator
#'
#' \code{lhs \%!>\% rhs} splices the elements of lhs into a function or call expression (\code{rhs}).
#'
#' The splice pipe takes a named list on the left hand side and inserts
#'   the arguments in the right with the rlang splice operator (\link[rlang]{!!!}).
#'   Unlike normal use of (\link[rlang]{!!!}), \code{\%!>\%} is compatable with
#'   functions that don't accept tidy dots.
#'
#' @param lhs List, with elements corresponding to arguments in rhs.
#' @param rhs A function or function call.
#' @return The results of rhs, with lhs spliced as arguments.
#' @export
#' @rdname splice_pipe
#' @examples
#'  list(1:20, na.rm = TRUE) %!>% sum
#' test_fun = function(a, b = 1, c = 2, d = 3) {
#'   paste0("a = ", a, ", b = ", b, ", c = ", c,", d = ", d)
#' }
#'  # These will produce the same result:
#'  list("A", b = "B") %!>% test_fun
#'  list("A", b = "B") %!>% test_fun()
#'
#'  # You can also specify arguments in the rhs function call
#'  list("A", b = "B") %!>% test_fun(d = 12)
#'
#'  # The lhs positional arguments take precedence, with the
#'    # rhs positional arguments filling the next available slot
#'  list("A", b = "B") %!>% test_fun("This is C", d = 12)
#'
#'  # Named arguments on the rhs take precedence and produce a warning
#'  list("A", b = "B", d = "nope") %!>% test_fun("C", d = 12)
#'
#'\dontrun{
#'  # This will fail because the "C" argument can't go anywhere
#'  list(a = "A", b = "B", c = "nope") %!>% test_fun("C", d = 12)
#'  }
#'
#'  # You can use annonymous functions,
#'  #   but they need to be wrapped in braces
#'  list(a = "A", b = "B") %!>% {function(a, b, ...) paste(a,b)}
#'
#'    # rlang lambda functions also need to be wrapped in braces
#'  list(.x = "A", .y = "B") %!>% {~paste(.x, .y)}
#'
#'  # The splice pipe can also be used to work with S3 outputs
#'\dontrun{
#'  lm(Sepal.Length ~ Sepal.Width, data = iris) %!>% {
#'    function(fitted.values, residuals, ...){
#'      plot(fitted.values, residuals)
#'      invisible(list(...))
#'    }
#'  } }
#'
#'  # You can force the evaluation of an rhs function factory
#'  # before splicing by wrapping it in parentheses.
#'  # The resulting function will then be spliced.
#'  foo = function(a) {
#'    function(b, c=5) {a + b + c}
#'  }
#'  foo(a = 1)(2) # is the same as
#'  list(b = 2) %!>% (foo(a = 1))
#'  foo(3)(2,9) # is the same as
#'  list(b = 2) %!>% (foo(3)(c = 9))

`%!>%` =
  function(lhs, rhs) {
    # Figure out how to use functions, no calls
    rhs_quo = validate_rhs(rlang::enquo(rhs), rhs)
    rhs_expr = rlang::get_expr(rhs_quo)
    eval_env = rlang::get_env(rhs_quo)
    if(!rlang::is_list(lhs)) rlang::abort("lhs must be a list for %!>%")
    # Precedence goes to positional args on the left,
    # but to named args on the right
    # manually supplied named arguments
    manual_args = rlang::call_args(rhs_expr)
    lhs_call = rlang::call_standardise(
      rlang::call2(rhs_expr[[1]], !!!lhs), env = eval_env)
    out_call = rlang::call_standardise(
      rlang::call_modify(lhs_call, !!!manual_args,
                         .homonyms = "last"), env = eval_env)
    # This way, manual args will overwrite piped arguments
    check_arg_name_overlap(lhs, manual_args, rhs_expr)
    out = rlang::new_quosure(out_call, env = eval_env)
    rlang::eval_tidy(out)
  }
