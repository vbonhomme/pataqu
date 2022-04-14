#' Predictor function to summarise results after shaking
#'
#' These functions are used internally by `diachrony_` and `synchrony_` functions,
#' but they may be of interest for custom constructions. See Details.
#'
#' @param df [tibble()], typically after [shake]. Must have a .x and and .y columns.
#' @param x_prediction numeric vector on x values on which to predict
#' @param ... additional parameters for each predictors (see Details)
#'
#' @details
#' In `df`, `x_new` and `y` columns must be present. This ease and fasten permutations
#' because the raw data is `prepare`d once for all.
#'
#' Most of these functions are thin wrappers around built-in  predictors such as
#' [stats::loess()], [stats::lm()], [mgcv::gam()], etc.
#'
#' These predictors follow these steps:
#'
#' 1. fit the `y` (value of interest) using the (typically shaked) `x`. We thus obtain
#' a continuous model that we use to:
#' 2. predict new `y` values on a range of `x`. These `x` values are typically fixed, so that
#'  we can later obtain confidence intervals, etc. for each of these points.
#'
#' They thus aim at summarising new `y` values at fixed `x` values, using
#'
#' `predict_bin` makes no intermediate
#' adjustment and summarises raw value either using mean or median (see Examples)
#'
#' @examples
#'
#' # replicability
#' set.seed(2329)
#' x_pred <- 1:100
#' df <- data.frame(x_new=x_pred, y=runif(100, -1, 1) + x_pred/100)
#' x_pred <- 1:100
#' p_loess  <- predictor_loess(df, x_pred) # default span, ie 0.75
#' p_loess2 <- predictor_loess(df, x_pred, span=0.2) # custom span
#'
#' # or using gam instead
#' p_gam <- predictor_gam(df, x_pred)
#' # or lm
#' p_lm  <- predictor_lm(df, x_pred)
#'
#' plot(df$x_new, df$y)
#' lines(p_loess$x_new, p_loess$y, col="firebrick3")
#' lines(p_loess2$x_new, p_loess2$y, col="orange")
#' lines(p_gam$x_new, p_gam$y, col="blue")
#' lines(p_lm$x_new, p_lm$y, col="forestgreen")
#'
#' ### bins
#' # when using bins the output depends of the function
#' p_breaks <- predictor_bins_breaks(df, x_pred)
#' # this returns factors
#' p_breaks
#' # and their natural graphical output is a boxplot
#' p_breaks %>% plot()
#'
#' # but this one returns midpoint as a numeric
#' p_midpoint <- predictor_bins_midpoint(df, x_pred)
#' # and the binning is quite clear here
#' plot(df$x_new, df$y)
#' points(p_midpoint$x_new, p_midpoint$y, col="blue", pch=20)
#' @name predictor
NULL

#' @describeIn predictor predict using [stats::loess()]
#' @export
predictor_loess <- function(df, x_prediction, ...){

  # build the model with loess.control direct surface
  # to get interpolation
  # see ?loess
  mod <- stats::loess(y~x_new, data=df,
                      control=stats::loess.control(surface="direct"), ...)

  # prepare a tibble for newdata
  # (and with consistent names to please predict.loess)
  newdata <- tibble::tibble(x_new=x_prediction)

  # predict
  predicted <- stats::predict(mod, newdata)
  # add to newdata and return this beauty
  dplyr::mutate(newdata, y=predicted)
}

# mod <- mgcv::gam(y ~ s(x_new, bs = 'cs'), data=df,
#                  method = "REML")

#' @describeIn predictor predict using [mgcv::gam]
#' @export
predictor_gam <- function(df, x_prediction, ...){
  # build the model
  mod <- mgcv::gam(y ~ s(x_new, bs = 'cs'), data=df)

  # prepare a tibble for newdata
  # (and with consistent names to please predict.loess)
  newdata <- tibble::tibble(x_new=x_prediction)

  # predict
  predicted <- stats::predict(mod, newdata)
  # add to newdata and return this beauty
  dplyr::mutate(newdata, y=predicted)
}

#' @describeIn predictor predict using [stats::lm()]
#' @export
predictor_lm <- function(df, x_prediction, ...){
  # build the model with loess.control direct surface
  # to get interpolation
  # see ?loess
  mod <- stats::lm(y~x_new, data=df, ...)

  # prepare a tibble for newdata
  # (and with consistent names to please predict.loess)
  newdata <- tibble::tibble(x_new=x_prediction)

  # predict
  predicted <- stats::predict(mod, newdata)
  # add to newdata and return this beauty
  dplyr::mutate(newdata, y=predicted)
}

#' @describeIn predictor no fit, just [cut()] and returns slices as factor
#' @export
predictor_bins_breaks <- function(df, x_prediction, ...){
  # not sure but so far the best way to ensure all fall within a cutting slice
  x_prediction <- unique(c(min(df$x_new), x_prediction, max(df$x_new)))
  # bin and return
  dplyr::mutate(df, x_new=cut(x_new, x_prediction, include.lowest = TRUE))
}

#' @describeIn predictor no fit, just [cut()] and returns slices as numeric
#' @export
predictor_bins_midpoint <- function(df, x_prediction, ...){
  # not sure but so far the best way to ensure all fall within a cutting slice
  x_prediction <- unique(c(min(df$x_new), x_prediction, max(df$x_new)))
  # mid points of intervals
  x_mid <- (x_prediction[-length(x_prediction)] + x_prediction[-1])/2
  # bin and return
  dplyr::mutate(df, x_new=x_new %>%
                  cut(x_prediction, labels=x_mid, include.lowest = TRUE) %>%
                  # not so elegant
                  as.character() %>% as.numeric())
}





