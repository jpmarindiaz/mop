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

  # Dataframes
  v <- c("first","second","1st","2nd","one","two")
  sv <- spread_every(v,2, into = c("F","S"))
  expect_equal(v[c(2*1:(length(v)/2))], sv[[2]])

  d <- diag(x=1:3,3,4)
  d[d==0] <- NA
  rbind(d,d)

  # Approx match
  dic <- data_frame(a = c('rat','tan','ken'), b = c(1,2,3))
  v <- c('rata','ten','kena')
  match_replace_approx(v, dic, max_dist = 0.3)
  match_replace_approx("xxx", dic)
  out <- match_replace_approx("xxx", dic, force = FALSE)
  expect_equal(out$.dist, NA)
})




