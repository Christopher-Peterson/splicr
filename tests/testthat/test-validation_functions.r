test_that("named_elements works correctly", {
  expect_equal(named_elements(list(a = 1, 2, b = 3)),
               c("a", "b"))
  expect_equal(named_elements(list(1, 2, 3, 4, 5)),
               character(0))
})
test_that("check_arg_name_overlap works", {
  expect_invisible(check_arg_name_overlap(
    list(a = 1, b = 2, 3, 4), list(c = 3, d = 4, 5),
      expr("tst")))
  expect_invisible(check_arg_name_overlap(
    list(3, 4), list(5), expr("tst2")))
  expect_invisible(check_arg_name_overlap(
    list(3, 4), list(), expr("tst3")))
  expect_invisible(check_arg_name_overlap(
    list(), list(5, 6), expr("tst2")))
  expect_warning(check_arg_name_overlap(
    list(a = 1, b = 2, c = 3, d = 4),
    list(c = 3, d = 4, a = 5), expr("tst_warn")))
})
test_that("check_braces works",{
  expect_true(check_braces(rlang::expr( {foo(x)} )))
  expect_false(check_braces(rlang::expr( foo(x) )))
  expect_false(check_braces(rlang::expr( foo )))
})
test_that("check_paren works",{
  expect_true(check_paren(rlang::expr( (foo(x)) )))
  expect_false(check_paren(rlang::expr( foo(x) )))
  expect_false(check_paren(rlang::expr( foo )))
})
test_that("drop_braces works for both braces and paren", {
  expect_equal(drop_braces(expr( {~.x + .y} )), expr(~.x + .y))
  expect_equal(drop_braces(expr( (~.x + .y) )), expr(~.x + .y))
  expect_equal(drop_braces(expr( {foo(bar)} )), expr(foo(bar)))

  expect_error(drop_braces(expr( c(a, b) )))
  expect_error(drop_braces(expr( foo )))
})
test_that("is_function_def works", {
  expect_true(
    is_function_def(expr( ~.x + .y )))
  expect_true(
    is_function_def(expr( function(a, b) {a + b} )))
  expect_false(
    is_function_def(expr( mean )))
  expect_false(
    is_function_def(expr( foo(bar) )))
})
test_that("assert_lambdas_in_braces rejects bad lambdas", {
 alib = assert_lambdas_in_braces
 expect_error(alib(expr( ~.x + .y )))
 expect_error(alib(expr( function(.x, .y) {.x + .y} )))
 expect_invisible(alib(expr( {~.x + .y} )))
 expect_invisible(alib(expr( {function(.x, .y) {.x + .y}} )))
 expect_invisible(alib(expr( mean )))
 expect_invisible(alib(expr( mean(1,2) )))
})

expect_quo_equal = function(object, expected_expr, expected_env) {
  obj_env_list = as.list(get_env(object))
  # browser()
  expect_mapequal(obj_env_list, as.list(expected_env))
  expect_equal(get_expr(object), expected_expr)
}

factory = function(a=100) function(b, c=1) {a + b + c}

test_that("eval_paren works for parentheses", {

  test_fun1= rlang::quo( (factory(a=4)(2)) )
  out1 = eval_paren(test_fun1, (factory(a=4)(2)))
  expect_equal(eval_tidy(out1), 7)

  test_fun2 = rlang::quo( (factory(a=4)) )
  out2 = eval_paren(test_fun2, (factory(a=4)))
  alt_factory = call2(factory())
  expect_quo_equal(out2, rlang::enexpr(alt_factory), list(a=4))
})
test_validate_rhs = function(rhs) {
  validate_rhs(rlang::enquo(rhs), rhs)
}
test_that("validate_rhs works: braces", {
  # Look at names?
  here = rlang::current_env()
  # Okay, this doesn't seem to be working correctly
  lambda_ex = rlang::as_function(~.x + .y)
  lambda_ex_call = call2(rlang::enexpr(lambda_ex))
  test_validate_rhs( {~.x + .y} ) %>%
    expect_quo_equal(lambda_ex_call, here)
  test_validate_rhs( (~.x + .y) )  %>%
    expect_quo_equal(lambda_ex_call, here)
  expect_error(test_validate_rhs( ~.x + .y))
})
test_that("validate_rhs works: factories", {
  here = rlang::current_env()
  fac1 = factory(1)
  fac2_foo = call2(rlang::enexpr(fac1), 2, 3)
  test_validate_rhs( {factory(1)} ) %>%
    expect_quo_equal(expr(factory(1)), here)
  test_validate_rhs( {factory(1)(2,3)} ) %>%
    expect_quo_equal(expr(factory(1)(2,3)), here)
  test_validate_rhs( (factory(1)(2,3)) ) %>%
    expect_quo_equal(fac2_foo, here)
  test_validate_rhs( (factory(1)) ) %>%#get_env %>% env_names
    expect_quo_equal(call2(fac1 ), list(a =1))
 })
test_that("validate_rhs works: regular functions", {
  here = rlang::current_env()
  test_validate_rhs( {mean} ) %>%
    expect_quo_equal(expr(mean()), here)
  test_validate_rhs( {mean()} ) %>%
    expect_quo_equal(expr(mean()), here)
  test_validate_rhs( mean(x) ) %>%
    expect_quo_equal(expr(mean(x)), here)
test_validate_rhs( (function(x) x) ) %>%
    expect_quo_equal(call2(function(x) x ), here)

# Problems here:::
  test_validate_rhs( (factory) ) %>%
    expect_quo_equal(call2(factory()), list(a = 100))
 # We Found an error:
})

# Test some arguments named 'c'
