context("mop")

test_that("Mop", {


  # Create slug

  create_slug("Bob Dylan")
  x <- "Erdös-Rényi-"
  expect_equal(create_slug("Erdös-Rényi-"),"erdos-renyi")


})




