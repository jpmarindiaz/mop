context("mop")

test_that("Mop", {
  x <- c("900.352.532-1","434053 ,544 - 1")
  expect_equal(clean_nit(x),c("900352532","434053544"))

  x <- c("luis juan prado gómez")
  expect_equal(mix_names_es(x)[[2]],c("luis prado","luis juan prado","luis prado gómez"))

  x <- c("CC 23934243", "Cedula Ciudadanía 43.342.432")
  clean_cc(x)

  x <- c("Company   SAS ", "Comp S. A. S.", "CASAS sas", "SASrorSAS SAS",
         "Pepe SA", "SASA SA", "hola Limitada")
  clean_company_name(x, context = "co")

})




