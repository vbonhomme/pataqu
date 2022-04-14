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
#' pred_075 <- predictor_loess(df, x_pred)
#' pred_05 <- predictor_loess(df, x_pred, span=0.5)
#' plot(df$x_new, df$y)
#' lines(pred_075$x_new, pred_075$y, col="red")
#' lines(pred_05$x_new, pred_05$y, col="blue")
#'
#' @name predictor
NULL




#' @describeIn predictor predict using loess
#' @export
# given a df (typically a shaked df),
# calculate a loess model with `...` arguments
# and return a tibble of predictions for `x_prediction` values
# for the sake of clairity we dont pipe a lot here
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



