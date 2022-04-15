test_that("prepare quake works", {
  # all arguments must be present
  expect_error(quake_uniform(), '"df" is missing')
  expect_error(animals %>% quake_uniform(), '"tpq" is missing')
  expect_error(animals %>% quake_uniform(tpq), '"taq" is missing')
  expect_error(animals %>% quake_uniform(taq, tpq), '"y" is missing')

  # inverted taq and tpq
  expect_error(expect_message(pb %>% quake_uniform(taq, tpq, value), "inverted"))

  # NAs and posterior
  pb <- animals
  pb$taq[c(4, 5)] <- NA
  pb$tpq[c(6, 7)] <- NA
  pb$tpq[c(8, 9)] <- Inf
  expect_error(expect_message(pb %>% quake_uniform(tpq, taq, value), "NA.* 4, 5, 6, 7"))
  expect_error(expect_message(pb %>% quake_uniform(tpq, taq, value), "tpq.*posterior 8, 9"))
})

test_that("quake returns groups or not but tibbles", {
  mini_an <- animals %>%
    dplyr::group_by(taxa) %>%
    dplyr::sample_n(10) %>%
    dplyr::ungroup()
  x1 <- mini_an %>% quake_uniform(tpq, taq, value, k=10)
  expect_s3_class(x1, 'tbl_df')
  expect_false("group" %in% colnames(x1))
  expect_equal(sort(unique(x1$k)), 1:10)

  x1 <- mini_an %>% quake_uniform(tpq, taq, value, group=taxa, k=2,
                                      x_prediction=10)
  expect_s3_class(x1, 'tbl_df')
  expect_true("group" %in% colnames(x1))
  expect_equal(sort(unique(x1$k)), 1:2)
  expect_equal(length(unique(x1$x_new)), 10)

})

