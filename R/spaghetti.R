#' Spaghetti plot after permutations
#'
#' ggplot2 primer for displaying the results of permutations
#'
#' @param x the result of a permutation function
#' @param ... additional parameters to [ggplot2::geom_line()]
#'
#' @examples
#' animals_q %>%
#' spaghetti()
#'
#' # you can easily customize it
#' # color palette from https://www.colourlovers.com/palette/1473/Ocean_Five
#' colors <- c("bird"="#00A0B0","cat"="#CC333F", "frog"="#CBE86B", "mouse"="#EDC951")
#' library(ggplot2)
#' animals_q %>%
#' spaghetti(alpha=0.5, size=0.1) +
#'   scale_color_manual(values=colors) +
#'   xlab("year") + ylab("value of interest") +
#'   guides(colour=guide_legend("taxa", override.aes=list(alpha=1, size=2)))
#' @export
spaghetti <- function(x, ...){
  x %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=x_new, y=y) +
    ggplot2::geom_line(...) +
    ggplot2::theme_minimal() -> gg

  # tricky way here for grouping + col but interaction does the job
  if ("group" %in% colnames(x))
    gg <- gg + ggplot2::aes(colour=group, group=interaction(k, group))
  else
    gg <- gg + ggplot2::aes(group=k)

  # print this beauty
  gg
}
