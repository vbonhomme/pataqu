#' Generate new datasets with new values
#'
#' This is a wrapper around [shakers](shake()) that allows to run it `k` times
#' and adds and index to the resulting tibble. As the typical entry point in permutationnal
#' analysis it also checks your data to avoid further problems.
#'
#' @param x [tibble()]
#' @param k number of new datasets to create
#' @param shaker one of the [shaker](shake()) functions ([shake_uniform()] by default)
#' @param ... arguments expected by the selected [shaker](shake()) function. They
#' must be provided and named. See examples
#'
#' @return  a 'shaken' [tibble()] with new columns and iteration index
#' @return
#' @export
#'
#' @examples
#' set.seed(2329) # replicability
#' # shaking uniform
#' df_u
#' df_u %>% quake(k=5, shake_uniform, min=tpq, max=taq)
#' # not that you can omit shakers' argument names, providing they come in the right order
#' # eg:  df_u %>% quake(k=5, shake_uniform, tpq, taq)
#'
#' # shaking gaussian
#' df_g
#' df_g %>% quake(k=5, shake_gaussian, mean=c14, sd=sd1)
#'
#' # on a more realistic dataset bigger baby:
#' # animals %>% quake(k=2, shake_uniform, tpq, taq)
#' @export
quake <- function(x, k=1, shaker=shake_uniform, ...){
  # so far, checking is lost but hereis how to do it
  # # args <- enquos(...)
  # eval(parse(text=paste0(".check_", substitute(shaker), "(args)")))
  # .check_shake_uniform <- function() cat("boo")

  # check
  message(" * quake ", substitute(x), " using ", substitute(shaker))
  # assemble an internal function
  # for not using ~ above
  # see https://stackoverflow.com/questions/71904154/is-quosurex-error-when-forwarding-inside-map
  f <- function(k){
    pb$tick()# tick the progress bar
    x %>%
      shaker(...) %>%
      dplyr::mutate(k=k, .before=dplyr::everything())
  }

  # initiate a progress bar
  message(" * launching ", k, " permutations")
  pb <- progress::progress_bar$new(
    format = " * shaking data [:bar] :percent eta: :eta",
    total = k, clear = TRUE, width= 60)

  # run and return
  purrr::map_dfr(seq_len(k), f)
}

# .check_shake_uniform <- function() cat("boo")
# .check_quake <- function(df, args){
#   cols_str <- purrr::map_chr(args, as_label)
#   existence <- purrr::map_lgl(cols_str, ~.col_exists(df, .x))
#   if (!all(existence))
#     stop("please fix that")
#
#   only_cols <- dplyr::select(df, !!!args)
#   #test all numeric, no NA, tpq < taq
#   # names of min, max omitted or exact
# }
