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
<<<<<<< HEAD
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
=======
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
>>>>>>> 077cbbb829fa313e3613254be5580844eb4c0bb3
  message(" * launching ", k, " permutations")
  pb <- progress::progress_bar$new(
    format = " * shaking data [:bar] :percent eta: :eta",
    total = k, clear = TRUE, width= 60)

<<<<<<< HEAD
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
=======
  # run and return
  purrr::map_dfr(seq_len(k), f)
>>>>>>> 077cbbb829fa313e3613254be5580844eb4c0bb3
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



