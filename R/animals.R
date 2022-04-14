#' Toy dataset of archaeological measurements
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

#' Animals with 100 permutations
#'
#' See [animal]. This is obtained with:
#' ```
#' set.seed(2329)
#' animals100 <-  diachrony_uniform(animals, tpq, taq, value, k=100, group=taxa)
#'```
#'
"animals100"

