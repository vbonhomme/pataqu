#' Spaghetti plot after permutations
#'
#' ggplot2 primer on the result of [quake] for displaying the results of permutations.
#' This allows inspecting the effect of [predictor]s.
#'
#' @param df [tibble()] the result of a permutation function
#' @param x,y colnames for x and y columns (`x_new`/`y` by default)
#' @param group colnames for grouping column (optionnal)
#' @param ... additional parameters to [ggplot2::geom_line()]
#'
#' @examples
#' spaghetti(animals_q, x=x_new, y=value, group=taxa)
#'
#' # of course you can filter this:
#' animals_q %>%
#'   dplyr::filter(taxa %in% c("frog", "cat")) %>%
#'   spaghetti(y=value, group=taxa)
#'
#' # or plot a single one
#' animals_q %>%
#'   dplyr::filter(taxa == "mouse") %>%
#'   spaghetti(y=value)
#'
#' # you can easily customize it
#' # with regular ggplot2's grammar
#'
#' # color palette from https://www.colourlovers.com/palette/1473/Ocean_Five
#' colors <- c("bird"="#00A0B0","cat"="#CC333F", "frog"="#CBE86B", "mouse"="#EDC951")
#' # if you library(ggplot2) you won't need all these 'ggplot2::'
#' animals_q %>%
#'   spaghetti(group=taxa, y=value, alpha=0.8, size=0.5) +
#'   ggplot2::scale_color_manual(values=colors) +
#'   ggplot2::labs(title="al dente", x="year", y="value of interest")
#' @export
spaghetti <- function(df, x=x_new, y=y, group, ...){
  df %>%
    # this will work either if group is missing or not
    # to define a grouping for geom_line
    dplyr::mutate(g=paste(k, {{group}}, sep="_")) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x={{x}}, y={{y}}, group=g) +
    ggplot2::geom_line(...) +
    ggplot2::theme_minimal() -> gg
  # if group is provided, then add colours
  if (!missing(group))
    gg <- gg + ggplot2::aes(col={{group}})
  # print this beauty
  gg
}

