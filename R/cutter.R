#' Discretise numeric x values into categorical
#'
#' Thin wrappers around [ggplot2::cut_interval()] and friends.
#' Useful for defining fixed x values for [fitting] functions.
#'
#' Here, columns of interest may be passed directly.
#' If you want to cut directly on numeric vectors,
#' use [base::cut()] or [ggplot2::cut_interval()] and friends.
#'
#' @param df a [tibble()]
#' @param x a cutter on which to extract breaks
#' @param n number of intervals to create or the approximate number of observations
#' @param width width of intervals to create
#' @param ... colnames
#'
#' @return factor with appropriate levels
#' @name cutter
#'
#' @examples
#'
#' cutter_interval(animals, 30, tpq, taq) %>% table()
#' cutter_number(animals, 10, tpq, taq,) %>% table()
#' cutter_width(animals, 30, tpq, taq) %>% table()
#'
#' # # note that in x_cut_number above, we have many observations yet
#' # the number of _different_ levels is not so diverse,
#' # so that n=30 would fail with "Insufficient data values"
#' # unlist(animals[, c("tpq", "taq")]) %>% table()
NULL

#' @describeIn cutter makes groups with equal range
#' @export
cutter_interval <- function(df, n, ...){
  df %>%
    dplyr::select(!!!enquos(...)) %>%
    tibble::deframe() %>%
    ggplot2::cut_interval(n=n)
}

#' @describeIn cutter makes n groups with approximately equal numbers of observations
#' @export
cutter_number <- function(df, n, ...){
  df %>%
    dplyr::select(!!!enquos(...)) %>%
    tibble::deframe() %>%
    ggplot2::cut_number(n=n)
}

#' @describeIn cutter makes groups of a certain width
#' @export
cutter_width <- function(df, width, ...){
  df %>%
    dplyr::select(!!!enquos(...)) %>%
    tibble::deframe() %>%
    ggplot2::cut_width(width=width)
}

#' @describeIn cutter extract breaks from a cutter factor
#' @export
cutter_to_seq <- function(x){
  # pattern from https://stackoverflow.com/questions/32356108/output-a-numeric-value-from-cut-in-r
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  # extract levels
  y <- levels(x)
  # c the two bounds
  c(gsub(pattern,"\\2", y), gsub(pattern,"\\3", y)) %>%
    # polish and return
    as.numeric() %>% unique() %>%
    sort()
}
