
test_that("shake works fine", {
  x <- df_u %>% shake_uniform(tpq , taq)
  expect_s3_class(x, "tbl_df")

})

