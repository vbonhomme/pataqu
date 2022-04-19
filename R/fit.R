#' Fit each permuations using various regression models
#'
#' Useful for boiling down the multiple permutations after [quake()].
#'
#' @details
#' `x_pred` can be passed directly as a vector of numeric. By default,
#' `cutter_number(df, 30, x_new)` is used so that 30 groups of approximately
#' equal numbers of observations are made using the new x values, ie likely to cover
#' the range of possible x values.
#'
#' `formula_rhs` correspond to the right hand side of the `formula`. It you want to
#' fit using, say `y~x_new`, this corresponds only to `x_new`. This saves typing `y~`
#' because it is already provided by the `y` argument. You can pass any [stats::formula()] you want.
#' For instance `x_new^2 + x_new -1` would correspond to a quadratic model with no intercept.
#' See [stats::formula()] to stick with this grammar.
#'
#' @section Motivation:
#' These helpers wrap [stats::loess()], [stats::lm()], [mgcv::gam()] and make
#' the assumption that you know what you are doing: ie the models are appropriate
#' and their conditions respected. They all :
#'
#' 1. fit a model on each permutation (and group-wise if `by` is provided).
#' 2. predict new y values using [stats::predict] on this model and for fixed `x_pred` values.
#'
#'
#' The main motivation is to generalize on many permutations what you would do on original data:
#' find a model that fits them and try to represent trends using it. In practice, when you're
#' adding [ggplot2::geom_smooth] to a graph, that's exactly what you do, even if `ggplot2` can largely
#' take care of the modelling aspect.
#'
#' The second motivation is to boil down the randomized x values into fixed x values,
#' that you provide to the function with the argument `x_pred`, and for which
#' predicted `y` values will be calculated using [stats::predict]. The price to pay is
#' to fit models but the benefit is that you cant test differences between groups
#' more easily for each of these fixed `x` values, that discretize your temporal range.
#'
#' Besides testing, graphical comparisons are eased too, because desriptors such as
#' extrema, other intermediate quantiles, average, etc. can be calculated for these
#' fixed x-values. This is the purpose of the main [spaghetti] plot.
#'
#' The last (and not least) motivation is to assess the quality of such fitting functions.
#' Using `.keep_mod=TRUE` gives a glimpse to it by providing indices such as
#' the adjusted r^2 (for `lm` and `gam`), and the residual standard deviation (for `loess`)
#' for each permutation. But it also retain each model so that you can access other components
#' of interest such as coefficients, p-values, etc.
#'
#' Note that you do not need fitting to discretize values, you can also [bin()] them
#' on requested x slices. That still allows testing minus the model intermediate.
#' Finally, if you only want a graphical approach, you can directly use [spaghetti0]
#' directly after [quake()]
#'
#' @param df [tibble()] typically returned by [quake()]
#' @param y colname of the value of interest. Default to `y`.
#' @param formula_rhs the right hand-side of the [stats::formula()] to be used.
#' Depending on the function, different defaults are used. See **Usage** section above.
#' @param by colname for grouping structure (besides `k`). Default to `NULL`
#' @param x_pred the sequence of x for which to predict new values after fitting
#' @param k colname for iteration . Default to `k`.
#' @param .keep_mod logical whether to keep regression models and
#' index of fit quality. See **Details**
#' @param ... additional parameters to be passed to main fitting functions. See **Examples**
#'
#' @return a [tibble()]
#'
#' @name fitting
#' @examples
#'
#' # Show fitter on a single iteration
#' df <- animals_q %>% dplyr::filter(k==1, taxa=="cat")
#' plot(df$x_new, df$value, pch=20, cex=0.2)
#' x_pred <- seq(-100, 100, 10)
#'
#' # use fitting functions
#' lm_fit    <- fit_lm(df, y=value, x_pred=x_pred, span=0.5) # you can specify arguments
#' loess_fit <- fit_loess(df, y=value, x_pred=x_pred)
#' gam_fit   <- fit_gam(df, y=value, x_pred=x_pred)
#'
#' # now draw them
#' lines(lm_fit$x_pred, lm_fit$y_pred, col="firebrick3")
#' lines(loess_fit$x_pred, loess_fit$y_pred, col="orange")
#' lines(gam_fit$x_pred, gam_fit$y_pred, col="blue")
#'
#' # usually, you would use them on a full object returned by quake
#' # here we show the first 6 permutations along with their loess
#' cat <- animals_q %>% dplyr::filter(k<=6, taxa=="cat")
#' cat_lines <- cat %>% fit_loess(y=value, x_pred=x_pred)
#' ggplot2::ggplot() +
#'  ggplot2::geom_point(mapping=ggplot2::aes(x=x_new, y=value), data=cat, size=0.2) +
#'  ggplot2::geom_line(mapping=ggplot2::aes(x=x_pred, y=y_pred, group=k), data=cat_lines, col="red") +
#'  ggplot2::facet_wrap(~k, ncol=3) +
#'  ggplot2::theme_minimal()
#'
NULL

#' @describeIn fitting fitting using linear model
#' @export
fit_lm <- function(df, y=y, formula_rhs=x_new, by=NULL,
                   x_pred=cutter_number(df, 30, x_new),
                   k=k, .keep_mod=FALSE, ...){

  # additional parameters
  args <- list(...)
  if (length(args)>0){
    args_mess <- paste(",", purrr::imap(args, ~paste0(.y, "=", .x)), collapse=", ")
  } else {
    args_mess <- ""
  }

  # build formula
  formula <- rlang::new_formula(substitute(y),  substitute(formula_rhs))
  message(" * fitting with lm(", deparse(formula), args_mess, ")")

  # if a seq is not passed directly and a cutter is used,
  # turn it into a numeric
  if (is.factor(x_pred))
    x_pred <- cutter_to_seq(x_pred)

  # prepare new data
  x_pred_tibble <- tibble::tibble(x_new=x_pred)

  # here, define a dedicated function because of ... eval
  f <- function(.x) stats::lm(formula, data=.x, ...)

  # now group by iter, and by if not null and nest
  df %>%
    dplyr::group_by({{k}}, {{by}}) %>%
    tidyr::nest() %>%
    # calculate for each tibble in 'data'
    dplyr::mutate(
      mod     = purrr::map(data, f),       # a lm based on formula
      y_pred  = purrr::map(mod,  ~stats::predict(.x, x_pred_tibble)), # predicted y values
      x_pred  = list(x_pred),                                         # paste x_pred
      adj_r2  = purrr::map_dbl(mod, ~summary(.x)$adj.r.squared)) %>%  # and also adj r2
    # drop data, ungroup, unnest
    dplyr::select(-data) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(c(y_pred, x_pred)) -> res

  # by default drop stats
  if (!.keep_mod)
    res <- dplyr::select(res, -mod, -adj_r2)

  # return this beauty
  res
}

#' @describeIn fitting fitting using generalised additive models with smoothness estimation
#' @export
fit_gam <- function(df, y=y, formula_rhs=s(x_new, bs = 'cs'), by=NULL,
                    x_pred=cutter_number(df, 30, x_new),
                    k=k, .keep_mod=FALSE, ...){

  # additional parameters
  args <- list(...)
  if (length(args)>0){
    args_mess <- paste(",", purrr::imap(args, ~paste0(.y, "=", .x)), collapse=", ")
  } else {
    args_mess <- ""
  }

  # build formula
  formula <- rlang::new_formula(substitute(y),  substitute(formula_rhs))
  message(" * fitting with gam(", deparse(formula), args_mess, ")")

  # if a seq is not passed directly and a cutter is used,
  # turn it into a numeric
  if (is.factor(x_pred))
    x_pred <- cutter_to_seq(x_pred)

  # prepare new data
  x_pred_tibble <- tibble::tibble(x_new=x_pred)

  # here, define a dedicated function because of ... eval
  f <- function(.x) mgcv::gam(formula, data=.x, ...)

  # now group by iter, and by if not null and nest
  df %>%
    dplyr::group_by({{k}}, {{by}}) %>%
    tidyr::nest() %>%
    # calculate for each tibble in 'data'
    dplyr::mutate(
      mod     = purrr::map(data, f),                                  # a gam based on formula
      y_pred  = purrr::map(mod,  ~stats::predict(.x, x_pred_tibble)), # predicted y values
      x_pred  = list(x_pred),                                         # paste x_pred
      adj_r2  = purrr::map_dbl(mod, ~summary(.x)$r.sq)) %>%           # and also adj r2
    # drop data, ungroup, unnest
    dplyr::select(-data) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(c(y_pred, x_pred)) -> res

  # by default drop stats
  if (!.keep_mod)
    res <- dplyr::select(res, -mod, -adj_r2)

  # return this beauty
  res
}

#' @describeIn fitting fitting using local polynomial regression
#' @export
fit_loess <- function(df, y=y, formula_rhs=x_new, by=NULL,
                      x_pred=cutter_number(df, 30, x_new),
                      k=k, .keep_mod=FALSE, ...){

  # additional parameters
  args <- list(...)
  if (length(args)>0){
    args_mess <- paste(",", purrr::imap(args, ~paste0(.y, "=", .x)), collapse=", ")
  } else {
    args_mess <- ""
  }

  # build formula
  formula <- rlang::new_formula(substitute(y),  substitute(formula_rhs))
  message(" * fitting with loess(", deparse(formula), args_mess, ")")

  # if a seq is not passed directly and a cutter is used,
  # turn it into a numeric
  if (is.factor(x_pred))
    x_pred <- cutter_to_seq(x_pred)

  # prepare new data
  x_pred_tibble <- tibble::tibble(x_new=x_pred)

  # here, define a dedicated function because of ... eval
  f <- function(.x) stats::loess(formula, data=.x, control=stats::loess.control(surface="direct"), ...)

  # now group by iter, and by if not null and nest
  df %>%
    dplyr::group_by({{k}}, {{by}}) %>%
    tidyr::nest() %>%
    # calculate for each tibble in 'data'
    dplyr::mutate(
      mod     = purrr::map(data, f),                                  # a loess, see func above
      y_pred  = purrr::map(mod,  ~stats::predict(.x, x_pred_tibble)), # predicted y values
      x_pred  = list(x_pred),                                         # paste x_pred
      rse  = purrr::map_dbl(mod, ~summary(.x)$s)) %>%                # and also adj r2
    # drop data, ungroup, unnest
    dplyr::select(-data) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(c(y_pred, x_pred)) -> res

  # by default drop stats
  if (!.keep_mod)
    res <- dplyr::select(res, -mod, -rse)

  # return this beauty
  res
}



