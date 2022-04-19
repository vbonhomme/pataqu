# test_that("prepare quake works", {
#   # all arguments must be present
#   expect_error(quake_uniform(), '"df" is missing')
#   expect_error(quake_uniform(59), "data.frame")
#   expect_error(expect_message(animals %>% quake_uniform(y=foo), 'foo .* exist'))
#   expect_error(expect_message(animals %>% quake_uniform(group=foo2, y=value), 'foo2 .* exist'))
#   expect_error(expect_message(animals %>% quake_uniform(taq=yo, y=value), 'yo .* exist'))
#   expect_error(expect_message(animals %>% quake_uniform(tpq=yo2, y=value), 'yo2 .* exist'))
#   expect_error(expect_message(animals %>% quake_uniform(tpq, taq), '"y" .* exist'))
#
#   # inverted taq and tpq
#   expect_error(expect_message(pb %>% quake_uniform(taq, tpq, value), "inverted"))
#
#   # message no group
#
#   # NAs and posterior
#   pb <- animals
#   pb$taq[c(4, 5)] <- NA
#   pb$tpq[c(6, 7)] <- NA
#   pb$tpq[c(8, 9)] <- Inf
#   expect_error(expect_message(pb %>% quake_uniform(tpq, taq, y=value), "NA .* 4, 5, 6, 7"))
#   expect_error(expect_message(pb %>% quake_uniform(tpq, taq, value), "tpq.*posterior 8, 9"))
#
# })
#
# test_that("quake returns rational results", {
#   mini_an <- animals %>%
#     dplyr::group_by(taxa) %>%
#     dplyr::sample_n(5) %>%
#     dplyr::ungroup()
#   x1 <- mini_an %>% quake_uniform(tpq, taq, y=value, k=2)
#   # classes
#   expect_s3_class(x1, 'tbl_df')
#   expect_false("group" %in% colnames(x1))
#
#   # nb of classes
#   expect_equal(sort(unique(x1$k)), 1:2)
#
#   expect_message(quake_uniform(mini_an, tpq, taq, y=value,
#                                predictor_fun = predictor_none), "no group")
#   expect_message(quake_uniform(mini_an, tpq, taq, group = taxa, y=value, k=2,
#                                predictor_fun = predictor_none), "group.*taxa")
#
#   # outside range
#   expect_message(quake_uniform(mini_an,  tpq, taq, group = taxa, y=value, k=1,
#                                predictor_fun = predictor_lm,
#                                x_prediction=c(-5e5, 5e5), "outside.*range"))
#
#   # correct range_nb
#   expect_equal(quake_uniform(mini_an,  tpq, taq, group = taxa, y=value, k=1,
#                              predictor_fun = predictor_lm,
#                              x_prediction=47) %>%
#                  dplyr::pull(x_new) %>% unique() %>% length(), 47)
#
#   # correct renaming
#   funny_names <- mini_an %>%
#     dplyr::rename(TPQ=tpq, TAQ=taq, species=taxa, mes=value) %>%
#     quake_uniform(TPQ, TAQ,  mes, group=species, k=1, predictor_fun = predictor_lm) %>%
#     colnames()
#   expect_true(all(c("species", "mes") %in% funny_names))
#
# })
#
