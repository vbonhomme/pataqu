#' Generate new x values
#'
#' Used for each permutation to generate new x values.
#' Different shakers are available depending on the type of
#' uncertainties you have
#'
#' @param x [tibble()]
#' @param min_col colname to use for taq
#' @param max_col colname to use for tpq
#' @param mean_col colname to use for mean
#' @param sd_col colname to use for sd
#'
#' @return a 'shaked'  [tibble()]
#'
#' @examples
#'
#' animals %>% shake_uniform(tpq, taq)
#'
#' animals %>% dplyr::mutate(mean=0, sd=50) %>% shake_gaussian(mean, sd)
#'
#' @name shake
NULL

#' @describeIn shake uniform distribution
#' @export
shake_uniform <- function(x, min_col, max_col){
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x_new=stats::runif(n=1,
                                     min=!!enquo(min_col),
                                     max=!!enquo(max_col))) %>%
    dplyr::ungroup()
}

#' @describeIn shake gaussian distribution
#' @export
shake_gaussian <- function(x, mean_col, sd_col){
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x_new=stats::rnorm(n=1,
                                     mean=!!enquo(mean_col),
                                     sd=!!enquo(sd_col))) %>%
    dplyr::ungroup()
}

