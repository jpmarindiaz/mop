
devtools::document()
devtools::install()


library(mop)



casos <- read_csv("~/Desktop/casos_data.csv", col_types = cols(.default = "c"),n_max = 10000)

geo_vars <- c("db","ID","COD_DEPTO","DEPTO","COD_MUNI","MUNI",
              "T_CENPOB","CENPOB","COD_CENPOB",
              "VEREDA")
d <- casos[geo_vars]

## Veredas
veredas <- read_csv("inst/data/geo/co/divipola-veredas.csv")

dv <- d[c("db","ID","VEREDA")] %>% filter(!is.na(VEREDA))
v <- unique(dv$VEREDA)
dic <- veredas[c("NOMBRE_VER","CODIGO_VER")] %>% distinct(NOMBRE_VER, .keep_all = TRUE)

x <- match_replace_approx(v, dic, force = TRUE)
x




#
d %>% filter(!is.na(CENPOB)) %>% nrow()
divipola <- read_csv("inst/data/geo/co/DIVIPOLA_20160930.csv")













lorem <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
lorem2 <- "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"

d <- data_frame(a = c(1,2,NA,NA),
                b = c("x",NA,"y",NA),
                c = c(NA,NA,NA,NA),
                d = c(4,3,2,1),
                e = c(lorem,lorem2,lorem,lorem2))
which_all_na_rows(d)
which_any_na_rows(d)
which_all_na_cols(d)
which_any_na_cols(d)
s <- summarize_str(d)

# test when not numeric
d <- data_frame(let = letters)
summarize_str(d)



catVals <- unique_cats(d)
catVals <- unique_cats(d, as_long = FALSE)

##
dic <- data_frame(code= c("01","02","03"),
                  name = c("first","second","third"))

d <- data_frame(number = 1:4,
                code= c("01","01","02","04"),
                name = c("first","fisrt","second","third"))

which_not_in_dic(d,dic)
which_not_in_dic(d,dic, cols = 2:3)
which_not_in_dic(d,dic, cols = c("code","name"))

match_replace(d$code,dic)
match_replace(d$code,dic, force = FALSE)

#
d <- read_csv("~/Desktop/casos-tmp.csv")
sd <- summarize_str(d)


