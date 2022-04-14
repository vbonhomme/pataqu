#' Prepare a data.frame for permutations
#'
#' This allows to ease and fasten permutations because the raw data is
#' `prepare`d once for all. This is a thin wrapper around [dplyr::rename()] that
#' also checks conditions before permutations.
#'
#' @param df [data.frame()] to work on
#' @param tpq,taq colname to use for tpq and taq (when shaking_gaussian)
#' @param mean,sd colname to use for mean and sd (when shaking_uniform)
#' @param y colname to use for the value of interest
#'
#' @examples
#' # let's say animals had different names
#' x <- animals %>% dplyr::rename(ante=taq, post=tpq, lsi=value)
#' x
#' x %>% diachrony_uniform(ante, post, lsi)
#' @name prepare


diachrony_uniform <- function(df, tpq, taq, y){
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


  # select and rename
  ready <-  dplyr::select(df, tpq=!!enquo_tpq, taq=!!enquo_taq, y=!!enquo_y)

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

  ready
}


