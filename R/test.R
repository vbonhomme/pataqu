
#' Test differences within temporal slices
#'
#' Useful after [fitting] to test "synchronic" differences between groups.
#' Within a temporal unit :
#'
#'  * `test_globally` will test if at least one group differs from the others
#'  * `test_pairwise` will test all pairwise differences between
#' groups present at that time.
#'
#' @param df [tibble()] typically the result of [quake()]
#' @param x,y,group colnames to use. Default to `x_pred`/`y_pred`
#' @param by colname for the group to use. Default to `group`
#' @param test_fun function to pick among [comparison_testers](comparison testers).
#' By default `kruskal_p`/`wilcox_p` for `synchrony`/`synchrony_pw`, respectively.
#'
#' @examples
#'
#' # for the sake of speed
#' x <- animals %>%
#'   quake(5, min=tpq, max=taq) %>%
#'   fit_gam(y=value, by=taxa, x_pred=seq(-100, 100, 50))
#'
#' x %>% spaghetti(by=taxa)
#'
#' # global testing
#' x %>% test_globally(by=taxa)
#'
#' # pairwise testing
#' x %>% test_pairwise(by=taxa)
#'
#' # you can filter "significant" ones
#' alpha=0.01
#' x %>%
#'   test_pairwise(by=taxa) %>%
#'   dplyr::mutate(signif=p<alpha)
#'   # you can continue the pipe with
#'   # dplyr::filter(!signif) to only get not different
#'   # or
#'   # dplyr::filter(signif) to only the different ones
#'
#' # yet before, you probably need to adjust your alpha
#' # by the number of tests, ie do some Bonferroni correction
#' # the number of tests is simply dplyr::n()
#'
#' x %>%
#'   test_pairwise(by=taxa) %>%
#'   dplyr::mutate(alpha_adj=alpha/dplyr::n(),
#'                 signif=p<alpha_adj)
#'
#' @name test
NULL

#' @describeIn  test test for global differences
#' @export
test_globally <- function(df, x=x_pred, y=y_pred, by=group, test_fun=kruskal_p){

  # message about testing
  message(" * testing global differences within ", substitute(by),
          " along ", length(unique(dplyr::pull(df, {{x}}))), " slices ",
          "using ", substitute(test_fun))

  # now test
  df %>%
    # rename
    dplyr::rename(y := {{y}}, g := {{by}}) %>%
    # group by temporal slice and prepare nested tibbles
    dplyr::group_by({{x}}) %>%
    tidyr::nest() %>% #return()
    # get the p.value using the appropriate test
    dplyr::mutate(p=purrr::map_dbl(data, test_fun)) %>%
    # we no longer need data
    dplyr::select(-data) %>%
    # nor grouping
    dplyr::ungroup()
}

# Pairwise version. We need a little more preparation to handle each pairwise test
# Tibble rearrangement is delegated to .tidy_pw (see below)
#' @describeIn  test test for pairwise differences
#' @export
test_pairwise <- function(df, x=x_pred, y=y_pred, by=group, test_fun=wilcox_p){

  # message about testing
  message(" * testing differences between pairs of ", substitute(by),
          " along ", length(unique(dplyr::pull(df, {{x}}))), " slices ",
          "using ", substitute(test_fun))

  # now test
  df %>%
    # rename
    dplyr::rename(y := {{y}}, g := {{by}}) %>%
    # group by temporal slice and prepare nested tibbles
    dplyr::group_by({{x}}) %>%
    tidyr::nest() %>% #return()
    # prepare these tibbles for pw comparisons then unnest
    dplyr::mutate(data=purrr::map(data, .tidy_pw)) %>%
    tidyr::unnest(data) %>%
    # get the p.value using the appropriate test
    dplyr::mutate(p=purrr::map_dbl(data, test_fun)) %>%
    # we no longer need data
    dplyr::select(-data) %>%
    # nor grouping
    dplyr::ungroup()
}


#' Comparisons testers
#'
#' These testers are used internally by [test](testers) but may be of interest outside.
#'
#' @param df [tibble()] with two columns named `g` and `y`,
#' and with only two levels in `group`.
#'
#' @return numeric p.value (or `NA_real` if not enough data or groups)
#'
#' @details
#' These testers are thin wrappers around [stats::wilcox.test], [stats::kruskal.test]
#' and [stats::aov]. Type `wilcox_p` (no bracket) for how to build your own.
#'
#'
#' @examples
#' # dummy tibble
#' x <- tibble::tibble(g=rep(letters[1:2], each=10),
#'                    y=stats::runif(20, -1, 1))
#'
# # only 2 groups
#' wilcox_p(x)
#' aov_p(x)
#'
#' # on >2 groups
#' x$g[15:20] <- "c"
#' kruskal_p(x)
#' aov_p(x)
#' @name comparison_testers
NULL

#' @describeIn comparison_testers wilcoxon's rank test for pairwise differences
#' @export
wilcox_p <- function(df){
  # early return if not enough data or levels
  if ((nrow(df)<2) | length(unique(df$g))<2)
    return(NA_real_)
  # otherwise wilcox this and return p.value
  stats::wilcox.test(y~g, data=df, digits.rank = 7)$p.value
}

#' @describeIn comparison_testers kruskal's test for global differences
#' @export
kruskal_p <- function(df){
  # early return if not enough data or levels
  if ((nrow(df)<2) | length(unique(df$g))<2)
    return(NA_real_)
  # otherwise wilcox this and return p.value
  stats::kruskal.test(y~g, data=df)$p.value
}

#' @describeIn comparison_testers analysis of variance test for both global and pairwise differences
#' @export
aov_p <- function(df){
  # early return if not enough data or levels
  # early return if not enough data or levels
  if ((nrow(df)<2) | length(unique(df$g))<2)
    return(NA_real_)
  # otherwise wilcox this and return p.value
  stats::aov(y~g, data=df) %>%
    # now go get the p_value
    summary() %>% unlist() %>% `[`("Pr(>F)1") %>% as.numeric()
}

# domestic helpers to rearrange pw tibbles
# tidy each permutation
# We have sth like this:
# k       g   y
# <int>  <chr>   <dbl>
# 1      cat    -0.00370
# 1      bird    0.0108
# 1      mouse   0.0000283
# 1      frog   -0.00342
# 2      cat    -0.00408
# 2      bird    0.00933
# and we would like to have sth like:
# pw           data
# bird ~ cat   <tibble [20 × 2]>
# bird ~ frog  <tibble [20 × 2]>
# bird ~ mouse <tibble [20 × 2]>
# cat ~ frog   <tibble [20 × 2]>
# cat ~ mouse  <tibble [20 × 2]>
# frog ~ mouse <tibble [20 × 2]>
# with: a label, and a df with only 2 groups and y
# pw being all pairwise combinations

.tidy_pw <- function(x){
  x %>%
    # we nest a first time
    dplyr::group_by(g) %>%
    dplyr::summarise(y=list(y)) %>%
    # then expand by copying nest df
    tidyr::expand(tidyr::nesting(g1=g, y1=y),
                  tidyr::nesting(g2=g, y2=y)) %>%
    # we then remove i ~ i
    dplyr::filter(g1 != g2) %>%
    # create a sorted paste so that we have our label
    dplyr::mutate(pw=purrr::map2_chr(g1, g2, ~c(.x, .y) %>%
                                       sort %>%
                                       paste0(collapse=" ~ "))) %>%
    # that also serves to only keep 1 in (i ~ j, j ~ i)
    dplyr::distinct(pw, .keep_all = TRUE) %>%
    # there might be a smarter way yet not too bad
    dplyr::mutate(data1=purrr::map2(g1, y1, ~tibble::tibble(g=.x, y=.y)),
                  data2=purrr::map2(g2, y2, ~tibble::tibble(g=.x, y=.y))) %>%
    dplyr::mutate(data=purrr::map2(data1, data2, dplyr::bind_rows)) %>%
    dplyr::select(pw, data)
}
