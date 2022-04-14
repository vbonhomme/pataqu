test_that("prepare diachrony works", {
  # all arguments must be present
  expect_error(diachrony_uniform(), '"df" is missing')
  expect_error(animals %>% diachrony_uniform(), '"tpq" is missing')
  expect_error(animals %>% diachrony_uniform(tpq), '"taq" is missing')
  expect_error(animals %>% diachrony_uniform(taq, tpq), '"y" is missing')

  # inverted taq and tpq
  expect_error(expect_message(pb %>% diachrony_uniform(taq, tpq, value), "inverted"))

  # NAs and posterior
  pb <- animals
  pb[1, 3] <- NA
  pb[3, 1] <- NA
  pb$tpq[c(6, 7)] <- 50000
  pb %>% diachrony_uniform(tpq, taq, value)


})


pb
