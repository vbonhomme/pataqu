# test_that(".col_exists works", {
#   expect_true(.col_exists(iris, "Petal.Width"))
#   expect_message(expect_false(.col_exists(iris, "Petal.Area"), "not exist"))
# })
