test_fun = function(a, b = 1, c = 2, d = 3) {
  paste0("a = ", a, ", b = ", b, ", c = ", c,", d = ", d)
}
browser()
test_that("pipe works for bare functions", {
 expect_equal(list("A", b = "B") %!>% test_fun,
              "a = A, b = B, c = 2, d = 3")
})
test_that("pipe works for function calls w/o arguments", {
  expect_equal(list("A", b = "B") %!>% test_fun(),
               "a = A, b = B, c = 2, d = 3")
})
test_that("pipe works for function calls w/ arguments", {
  expect_equal(list("A", b = "B") %!>% test_fun(d = 12),
               "a = A, b = B, c = 2, d = 12")
})
test_that("Positional arguments defer to lhs", {
  expect_equal(list("A", b = "B") %!>%
                 test_fun("This is C", d = 12),
               "a = A, b = B, c = This is C, d = 12")
})
test_that("Named arguments defer to rhs", {
  expect_warning(expect_equal(
    list("A", b = "B", d = "nope") %!>% test_fun("C", d = 12),
   "a = A, b = B, c = C, d = 12"))
})
test_that("Splicing too many arguments fails", {
  expect_error(
    list(a = "A", b = "B", c = "nope") %!>% test_fun("C", d = 12))
})
test_that("Splicing invalid arguments fails", {
  expect_error(
    list(a = "A", b = "B", f = "nope") %!>% test_fun("C", d = 12))
})

test_that("Annonymous functions work w/ braces: ",{
 expect_equal("A B",
   list(a = "A", b = "B") %!>% {function(a, b, ...) paste(a,b)})
 expect_equal("A B",
              list(.x = "A", .y = "B") %!>% {~paste(.x, .y)})
})
test_that("rhs function factories work with parentheses", {
  foo = function(a) {
    function(b, c=5) {a + b + c}
  }
  # No manual args
  expect_equal(list(b = 2) %!>% (foo(a = 1)),
               foo(a = 1)(2))
  # manual args
  expect_equal(list(b = 2) %!>% (foo(3)(c = 9)),
               foo(3)(2,9))
  expect_equal(list(2) %!>% (foo(3)(c = 9)),
               foo(3)(2,9))
})

test_that("Check to see if it works inside functions", {
  closure_fun = function(z) {
    test_fun = function(a, b = "B") paste(a, b, z)
    list("A") %!>% test_fun("boy")
  }
  expect_equal(closure_fun("and his dog"), "A boy and his dog")
})
