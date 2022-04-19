#' Bin
#'
#' Bin and summarise without [fitting]
#'
#' @param df [tibble()] typically returned by [quake()]
#' @param y,x colnames to use. Default to `y`/`x_new`
#' @param by colname for grouping structure (besides `k`). Default to `NULL`
#' @param fun to use to summarise `y`
#' @param k colname for iteration . Default to `k`.
#'
#' @examples
#' animals_q %>% bin(y=value, by=taxa, fun=mean)
#'
#' animals_q %>% bin(y=value, fun=mean)
#' @export
bin <- function(df, y=y, x=x_new, by=NULL, fun=stats::median,
                      x_bin,
                      k=k){
  # todo implement
  # if (missing(x_pred))
  #   df %>% dplyr::pull({{x}}) %>% ggplot2::cut_number(5)

  df %>%
    dplyr::mutate(x_bin=ggplot2::cut_number({{x}}, 5)) %>%
    dplyr::group_by({{k}}, {{by}}, x_bin) %>%
    dplyr::summarise(y_bin=fun({{y}}), .groups = "drop")
}
