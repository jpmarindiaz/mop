context("mop")

test_that("Mop", {

  # Approx match
  dic <- data_frame(a = c('rat','tan','ken'), b = c(1,2,3))
  v <- c('rata','ten','kena')
  match_replace_approx(v, dic, max_dist = 0.3)
  match_replace_approx("xxx", dic)
  out <- match_replace_approx("xxx", dic, force = FALSE)
  expect_equal(out$.dist, NA)

  dic <- data_frame()
  expect_error(match_replace_approx("xxx", dic))

  # Match replace substr
  dic <- data_frame(a = c('rat','tan','ken'), b = c("RAT","TAN","KEN"))
  v <- c('fda fdsa rata','raet tan','fda ken')
  # dic must be of the same length as v
  match_replace_substr(v,dic)

  #discard
  d <- data_frame(x = c(1,NA), y = c(1,2), z = c("NA",NA))
  discard_any_na_cols(d)

  # Coalesce rows
  x <- data_frame(id = c("A","A","A","B", "B"), a = c(NA,0, 0, 0, NA), b = c(0,1,2,3, 1), c = c(NA,1,2,NA, 3))
  coalesce_rows(x, id, sep = "||")
  coalesce_rows(x, id, collapse_many = FALSE)


  })




