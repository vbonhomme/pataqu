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
quake_uniform <- function(df, tpq=tpq, taq=taq, y=y, group=group,
                          k=10,
                          predictor_fun=predictor_loess,
                          x_prediction=30, ...){

  ## checking, round 1
  # check df first
  if (missing(df))
    stop('"df" is missing')
  if (!is.data.frame(df))
    stop('"df" must be a data.frame or a tibble')

  # tidy eval now
  enquo_tpq   <- enquo(tpq)
  enquo_taq   <- enquo(taq)
  enquo_y     <- enquo(y)
  enquo_group <- enquo(group)

  # col checking to prevent terrible things
  .check_quake_cols(df, enquo_tpq, enquo_taq, enquo_y, missing(group), enquo_group)

  ## preparing
  # we use this select and renaming approach to
  # define custom shaker. There is probably a better way but this one
  # is not so bad though
  if (missing(group)){
    message(" * no group defined")
    # in case a group is not provided, we do not select but create a constant one
    # for downstream splitting
    ready <-  df %>%
      dplyr::select(tpq=!!enquo_tpq, taq=!!enquo_taq, y=!!enquo_y) %>% dplyr::mutate(group="foo")
  } else {
    message(" * grouping on ", as_label(enquo_group))
    # but if provided, of course, we collect it
    ready <- dplyr::select(df, tpq=!!enquo_tpq, taq=!!enquo_taq, y={{y}}, group=!!enquo_group)
  }

  ## checking round 2
  # checking, delegated to domestic .check_quake
  .check_quake_data(ready)

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

  ## permutations begins

  # because we're worth it, add a progress bar
  message(" * launching ", k, " permutations")
  pb <- progress::progress_bar$new(
    format = " * shaking data [:bar] :percent eta: :eta",
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

  ## final polish
  # add a permutation index and bind all rows
  res <- res %>%
    purrr::imap_dfr(~dplyr::mutate(.x, k=.y)) %>%
    # cosmetics: put the k as first column
    dplyr::relocate(k, group)

  # rename
  res <- dplyr::rename(res, !!enquo_group := group, !!enquo_y := y)

  # if no group was provided, drop the 0s
  if (missing(group))
    res <- dplyr::select(res, -!!enquo_group)

  # return this beauty
  res
}

# quite laborious but one day NSE will have no secrets for me
.check_quake_cols <- function(df, enquo_tpq, enquo_taq, enquo_y, missing_group, enquo_group){
  cols <- c(.col_exists(df, as_label(enquo_tpq)),
            .col_exists(df, as_label(enquo_taq)),
            .col_exists(df, as_label(enquo_y)),
            ifelse(missing_group, TRUE,
                   .col_exists(df, as_label(enquo_group))))
  if (!all(cols))
    stop("please fix this before permutations", call. = FALSE)
  # no return if fine
}

.check_quake_data <- function(x){
  ## checking begins
  # we prefer this approach so that we can notice bad lines
  checked <-  x %>%
    dplyr::transmute(
      na = apply(is.na(x), 1, any), # na flag
      d  = tpq>taq)                 # tpq > taq flag

  # some not satisfied and will end up with message, then stop
  if (any(checked)){
    if (any(checked$na))
      message(" * NA at lines c(", paste(which(checked$na), collapse = ", "), ")")

    if (any(checked$d, na.rm = TRUE)){
      if (mean(checked$d, na.rm=TRUE)>0.5)
        message(" * tpq is posterior to taq for most dates. Did you inverted tpq and taq?")
      else
        message(" * tpq is posterior to taq at lines c(", paste(which(checked$d), collapse = ", "), ")")
    }
    stop("please fix this before permutations", call. = FALSE)
  }
  # no return, only side effects
}
