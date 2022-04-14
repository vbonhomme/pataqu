
test_that("hello prints", {
  x <- tibble::tibble(tpq=1:10, taq=tpq+1, y=10:1) %>% shake_uniform()
  expect_s3_class(x, "tbl_df")
  expect_true(all((x$x_new>=x$tpq) & ((x$x_new<=x$taq))))

  x <- tibble::tibble(mean=0, sd=1:10, y=10:1) %>% shake_gaussian()
  expect_s3_class(x, "tbl_df")
})

