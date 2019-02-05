context("mop")

test_that("Strings", {
  s <- c("tools|what", "yes", "no way")
  words <- c("what", "yes")
  expect_equal(c(TRUE, TRUE, FALSE), contains_any(s, words))

})

