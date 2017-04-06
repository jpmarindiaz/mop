
devtools::install()
devtools::document()


library(mop)

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


