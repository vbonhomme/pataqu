#' Dataset of archaeological measurements with terminus post and ante quem dating
#'
#' This is a real but "anonymized" dataset of archaeological data.
#'
#' The original dataset aimed at measured the evolution of the `value` index,
#' of four `taxa`, on remains recovered in `us` within `site` wand with datations
#' provided as `tpq` and `taq`.
#'
#' @format A data frame with 5533 rows and 6 variables:
#'   * taxa 4 species names (imaginary)
#'   * site 64 site names (abbreviated)
#'   * us 1013 us (abbreviated), within these sites
#'   * tpq terminal post quem, ranging from -225 to 375
#'   * taq terminal ante quem, ranging from -175 to 700
#'   * value the parameter of interest measured
#'
#' @source Unpublished dataset, modified by the authors to become a toy dataset.
"animals"

#' Animals with 20 permutations on 7 temporal slices
#'
#' See [animals]. This is obtained with:
#' ```
#' set.seed(2329)
#' animals_q <-  animals %>%
#'   dplyr::filter(tpq>-100, taq<100) %>%
#'   quake(k=20, shaker=shake_uniform, tpq, taq)
#' usethis::use_data(animals_q, overwrite=TRUE)
#'```
#'
"animals_q"

#' Toy dataset with terminus ante and post quem dating
#'
#' Only used for examples
#'
#' This is obtained with:
#' ```
# set.seed(2329)
# df_u <- tibble::tibble(tpq  = round(runif(8, -100, 100)), # random tpq
#                        taq  = tpq+round(runif(8, 5, 50)), # taq based tpq+random
#                        species = rep(c("fox", "hound"), each=4), # dummy grouping
#                        site = rep(letters[1:2], each=2, times=2), # dummy sites
#                        mes  = c(1:4, 4:1) + runif(8, -0.5, 0.5)) # dummy value of interest
# usethis::use_data(df_u, overwrite=TRUE)
#' ````
"df_u"

#' Toy dataset with centered/interval dating
#'
#' Only used for examples
#'
#' This is obtained with:
#' ```
#' set.seed(2329)
#' df_g <- tibble::tibble(c14  = round(runif(8, -100, 100)), # best prediction
#'                        sd1   = round(runif(8, 5, 20)),     # one sd
#'                        species = rep(c("fox", "hound"), each=4), # dummy grouping
#'                         site = rep(letters[1:2], each=2, times=2), # dummy sites
#'                        mes  = c(1:4, 4:1) + runif(8, -0.5, 0.5))
#'usethis::use_data(df_g, overwrite=TRUE)
#' ```
"df_g"
