#' Spaghetti plot after permutations
#'
#' ggplot2 primer for displaying the results of permutations
#'
#' @param x the result of a permutation function
#'
#' @examples
#' x_loess <- animals %>%
#'   diachrony_uniform(tpq, taq, value, k=5,
#'   group=taxa, predictor_fun=predictor_loess)
#'
#' spaghetti(x_loess)
#'
#' # with a lm now
#' x_lm <- animals %>%
#'   diachrony_uniform(tpq, taq, value, k=5,
#'   group=taxa, predictor_fun=predictor_lm)
#'
#' spaghetti(x_lm)
#' @export
spaghetti <- function(x){
  x %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=x_new, y=y) +
    ggplot2::geom_path() +
    ggplot2::theme_minimal() -> gg

  # tricky way here for grouping + col but interaction does the job
  if ("group" %in% colnames(x))
    gg <- gg + ggplot2::aes(colour=group, group=interaction(k, group))
  else
    gg <- gg + ggplot2::aes(group=k)

  # print this beauty
  gg
}
