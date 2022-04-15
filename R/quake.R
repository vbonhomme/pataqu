#' Use permutations to visualize and test uncertainties on x
#'
#' More details here.
#'
#' @param df [data.frame()] to work on
#' @param tpq,taq colname to use for tpq and taq (when shaking_gaussian)
# #' @param mean,sd colname to use for mean and sd (when shaking_uniform)
#' @param y colname to use for the value of interest
#' @param k number of permutations (10 by default)
#' @param predictor_fun function one of [predictor]
#' @param x_prediction on which to predict new values. See Details.
#' @param group colname (optionnal) whether you have groups
#' @param ... additional arguments to `predictor_fun`
#'
#' @details If `x_prediction` can be passed as a single numeric or as
#' a vector of numeric. If a single numeric is passed, a regular sequence of this length
#' will be created between `min(tpq)` and `max(taq)`, ie covering the full temporal span.
#' If a vector of numeric is passed, then these values will be used. By default, it
#' eis `30`, so that 30 dates covering the temporal span will be used. See examples.
#'
#' @examples
#' set.seed(2329) # for replicability
#'
#' # let's say animals had different names
#' x <- animals %>% dplyr::rename(post=tpq, ante=taq, lsi=value)
#' x
#' x %>% quake_uniform(post, ante, lsi)
#'
#' animals %>% quake_uniform(tpq, taq, value, k=2, group=taxa)
#'
#' \dontrun{
#' library(ggplot2)
#' x %>% ggplot() +
#'   aes(x_new, y, group=k, col=group) +
#'   geom_point(size=0.3) +
#'   theme_minimal() +
#'  geom_smooth(aes(group=NULL, col=group))
#' }
#' @export
#' @aliases quake
quake_uniform <- function(df, tpq, taq, y, group,
                              k=10, predictor_fun=predictor_loess,
                              x_prediction=30, ...){
  # all arguments must be present
  if (missing(df))
    stop('"df" is missing')
  if (missing(tpq))
    stop('"tpq" is missing')
  if (missing(taq))
    stop('"taq" is missing')
  if (missing(y))
    stop('"y" is missing')

  # tidy eval now
  enquo_tpq <- enquo(tpq)
  enquo_taq <- enquo(taq)
  enquo_y   <- enquo(y)

  enquo_group <- enquo(group)

  # select and rename
  if (missing(group)){
    #in case a group is not provided, we do not select but create a constant one
    # for downstream splitting
    ready <-  dplyr::select(df, tpq=!!enquo_tpq, taq=!!enquo_taq, y=!!enquo_y) %>%
      dplyr::mutate(group=0)
  } else {
    # but if provided, of course, we collect it
    ready <- dplyr::select(df, tpq=!!enquo_tpq, taq=!!enquo_taq, y=!!enquo_y, group=!!enquo(group))
  }

  ## checking begins
  # we prefer this approach so that we can notice bad lines
  checked <-  ready %>%
    dplyr::transmute(
      na = apply(is.na(ready), 1, any), # na flag
      d  = tpq>taq) # tpq > taq flag

  # some not satisfied and will end up with message, then stop
  if (any(checked)){
    if (any(checked$na))
      message(" * NA at lines c(", paste(which(checked$na), collapse = ", "), ")")

    if (any(checked$d)){
      if (mean(checked$d, na.rm=TRUE)>0.5)
        message(" * tpq is posterior to taq for most dates. Did you inverted tpq and taq?")
      else
        message(" * tpq is posterior to taq at lines c(", paste(which(checked$d), collapse = ", "), ")")
    }

    stop("please fix this before permutations")
  }
  ## checking ends


  # prepare x_prediction
  domain <- range(c(ready$taq, ready$tpq))
  # missing or length case
  if (length(x_prediction)==1){
    message(" * working on a temporal sequence of ", x_prediction, " steps")
    x_prediction <- seq(min(domain), max(domain), length.out=x_prediction)
  }
  # check the domain of validity (if user-provided)
  if (min(x_prediction)<min(domain) | max(x_prediction)>max(domain))
    message(' * "x_prediction" requested is outside the range of dates observed in the data')

  # now permutates

  # because we're worth it, add a progress bar
  message(" * launching ", k, " permutations")
  pb <- progress::progress_bar$new(
    format = " shaking data [:bar] :percent eta: :eta",
    total = k, clear = FALSE, width= 60)

  # the workhouse map
  res <- purrr::map(1:k,
                    ~{
                      # tick on the master loop
                      pb$tick()
                      # internal loop that works  per group
                      ready %>% split(.$group) %>%
                        purrr::imap_dfr(
                          ~.x %>%
                            # shake and predict
                            shake_uniform() %>%
                            predictor_fun(x_prediction = x_prediction) %>%
                            # and dont forget the group
                            dplyr::mutate(group=.y)
                        )
                    }
  )


  res <- res %>%
    # add a permutation index and bind all rows
    purrr::imap_dfr(~dplyr::mutate(.x, k=.y)) %>%
    # cosmetics: put the k as first column
    dplyr::relocate(k, group)

  # if no group was provided, drop the 0s
  if (missing(group))
    res <- dplyr::select(res, -group)

  # return this beauty
  res
}

