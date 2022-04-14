#' Generate new x values
#'
#' Used internally for each permutation to generate new x values.
#' Different shakers are available depending on the type of
#' uncertainties you have.
#'
#' @param x [tibble()] typically obtained with [prepare()]
#' @return a 'shaked'  [tibble()]
#'
#' @details expects `tpq`, `taq` colnames for `shake_uniform`,
#' and `mean`, `sd` for `shake_gaussian`
#'
#' @examples
#' set.seed(2329) # replicability
#' tibble::tibble(tpq=c(-50, 100), taq=c(50, 200)) %>% shake_uniform()
#' tibble::tibble(mean=c(0, 100), sd=c(5, 20)) %>% shake_gaussian()
#'
#' @name shake
NULL

#' @describeIn shake uniform distribution
#' @export
shake_uniform <- function(x){
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x_new=stats::runif(n=1, min=tpq, max=taq)) %>%
    dplyr::ungroup()
}

#' @describeIn shake gaussian distribution
#' @export
shake_gaussian <- function(x){
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x_new=stats::rnorm(n=1,
                                     mean=mean,
                                     sd=sd)) %>%
    dplyr::ungroup()
}

