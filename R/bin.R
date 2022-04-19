#' Bin
#'
#' Bin and summarise without [fitting]
#'
#' @param df [tibble()] typically returned by [quake()]
#' @param y colname of the value of interest. Default to `y`.
#' @param fun to use to summarise `y`
#' @param by colname for grouping structure (besides `k`). Default to `NULL`
#' @param x_pred the sequence of x for which to predict new values after fitting
#' @param k colname for iteration . Default to `k`.
#'
bin <- function(df, y=y, fun=stats::median, by=NULL,
                      x_pred=cutter_number(df, 30, x_new),
                      k=k){

}
