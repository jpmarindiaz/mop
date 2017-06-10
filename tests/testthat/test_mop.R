context("mop")

test_that("Mop", {


  # Create slug

  create_slug("Bob Dylan")
  x <- "Erdös-Rényi-"
  expect_equal(create_slug("Erdös-Rényi-"),"erdos-renyi")

  # Strings
  expect_equal(totitle(c("TIFFANY ISAACS","tim mo.")),
               c("Tiffany Isaacs","Tim Mo."))
  expect_equal(totitle("JOHN BURK"),"John Burk")

  s <- 'fdasfa(in parenthesis).."in quotes" and \'in single quotes\''
  expect_equal(extract_inside_parenthesis(s),"in parenthesis")

  expect_equal(extract_inside_quotes(s), "in quotes")
  expect_equal(extract_inside_quotes(s, quoteMark = "'"),"in single quotes")
  expect_equal(extract_between_chars(s,'s'),'fa(in parenthe')
  expect_equal(extract_between_chars(s,'d','f'),'as')

  # Dates
  expect_equal(timestamp_date(-863715600),"1942-08-19")


})




