context("mop")

test_that("Strings", {
  s <- c("tools|what", "yes", "no way", NA)
  words <- c("what", "yes")
  contains_any_words(s, words)
  expect_equal(c(TRUE, TRUE, FALSE, FALSE), contains_any_words(s, words))

  s <- c("#", "Escribe tu nombre y apellido", "Escribe tu correo electrónico", "Start Date (UTC)", "Submit Date (UTC)", "Network ID")
  vars <- c("nombre","Email","email","correo","apellido")
  contains_any_words(s, vars)

  s <- c("carlos perez dias", "camila soto", "juan alfosno")
  extract_last_word(s, 2)


  s <- "rere,,,re,,,"
  remove_repeated_char(s)
  remove_repeated_char(s, pattern = ",")


})

