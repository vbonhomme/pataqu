#' Generate new x values
#'
#' Used internally by [quake()] to generate new x values. Different flavours (`shake_*`) exist
#' depending the type of uncertainties you have. `*_within` variants allow to share new values
#' within a level.
#'
#' @param x [tibble()]
#' @param min,max colnames to pass to [stats::runif()] (`shake_uniform_*`)
#' @param mean,sd colnames to pass to [stats::rnorm()] (`shake_gaussian_*`)
#' @param within colname to define which levels share new x values (`*_within` variants)
#'
#' @return a 'shaken' [tibble()] with an additional `x_new` column
#'
#' @details
#' `uniform` considers that the _actual_ x can be anytime between two temporal bounds.
#' This is typically the case for `ante quem` data.
#'
#' `gaussian` considers that the _actual_ x is centered on a value but with an associated error.
#' You can think of C^14 dating which is provided as and estimate and a standard deviation to expect
#' around it.
#'
#' For `shake_uniform_*`, `min`/`max` correspond to `tpq`/`taq` respectively.
#' For `shake_gaussian_*`, `mean`/`sd` correspond to the single (best) x and its standard deviation.
#'
#' @name shake
#' @examples
#' set.seed(2329) # replicability
#' # we will use builtin dummy df_u and df_g
#'
#' # show uniform shaking
#' df_u # 'unshaken' data
#' df_u %>% shake_uniform(tpq, taq)  # new x values bounded between each tpq/taq
#' df_u %>% shake_uniform(tpq, taq)  # same idea, different values
#'
#' # you can decide that new values must be stratified per site
#' # we create a df_u variant with equal tpq and taq per site (otherwise makes no sense)
#' df_u %>%
#'   dplyr::group_by(site) %>%
#'   dplyr::mutate(tpq=tpq[1], taq=taq[1]) %>% # first value defines all others
#'   dplyr::ungroup() -> df_u_site
#'
#' # now compare
#' df_u_site %>% shake_uniform(tpq, taq)
#' df_u_site %>% shake_uniform_within(tpq, taq, within=site) # equal x_new within site
#'
#' # gaussian shaking now
#' df_g # unshaken data
#' df_g %>% shake_gaussian(c14, sd1) # x_new is centered on c14 with gaussian noise (sd=sd1)
#' df_g %>% shake_gaussian(c14, sd1) # same idea, different values
#'
#' # stratified version
#' # same approach as above, inherit from 1st value,
#' # to make a tibble that makes sense for *_within variant
#' df_g %>%
#'   dplyr::group_by(site) %>%
#'   dplyr::mutate(c14=c14[1], sd1=sd1[1]) %>%
#'   dplyr::ungroup() -> df_g_site
#'
#' df_g_site %>% shake_gaussian_within(c14, sd1, site)
#'
NULL

#' @describeIn shake generate new x values using uniform distribution
#' @export
shake_uniform <- function(x, min, max){
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x_new=runif(n=1, min={{min}}, max={{max}})) %>%
    dplyr::ungroup()
}

#' @describeIn shake shake_uniform within a group
#' @export
shake_uniform_within <- function(x, min, max, within){
  x %>%
    dplyr::group_by({{within}}) %>%
    dplyr::mutate(x_new=runif(n=1, min={{min}}, max={{max}})) %>%
    dplyr::ungroup()
}

#' @describeIn shake generate new x values using gaussian distribution
#' @export
shake_gaussian <- function(x, mean, sd){
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x_new=stats::rnorm(n=1, mean={{mean}}, sd={{sd}})) %>%
    dplyr::ungroup()
}

#' @describeIn shake shake_gaussian within a group
#' @export
shake_gaussian_within <- function(x, mean, sd, within){
  x %>%
    dplyr::group_by({{within}}) %>%
    dplyr::mutate(x_new=stats::rnorm(n=1, mean={{mean}}, sd={{sd}})) %>%
    dplyr::ungroup()
}
