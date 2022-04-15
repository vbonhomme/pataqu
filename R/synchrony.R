
#' Test differences within temporal slices
#'
#' Useful after [quake()] to test "synchronic" differences between groups.
#' Within a temporal unit :
#'
#'  * `synchrony` will test if at least one group differs from the others
#'  * `synchrony_pw` will test all pairwise differences between
#' groups present at that time.
#'
#' @param df [tibble()] typically the result of [quake()]
#' @param x,y,group colnames to use
#' @param test_fun function to pick among [synchrony_testers()].
#' By default `kruskal_p`/`wilcox_p` for `synchrony`/`synchrony_pw`, respectively.
#'
#' @examples
#' (an1 <- animals_q %>% synchrony())
#' # pairwise testing and using aov here
#' (an2 <- animals_q %>% synchrony_pw(test_fun=aov_p))
#'
#' # you can easily filter where it differs with
#' alpha=1e-3
#' alpha_adj <- alpha/nrow(an1) # cheap Bonferroni correction
#' an1 %>% dplyr::filter(p < alpha_adj)
#'
#' # you can also easily recreate group columns
#' tidyr::separate(an2, col=pw, into=c("group1", "group2"), sep=" ~ ", remove=FALSE)
#'
#' # if your tibble have different names just mention them
#' animals_q %>%
#'   # rename to simulate different names
#'   dplyr::rename(taxa=group, year=x_new, value=y) %>%
#'   # you retain your names
#'   synchrony(year, value, taxa)
#'
#' @export
synchrony <- function(df, x=x_new, y=y, group=group, test_fun=kruskal_p){
  # tidy eval
  x_enquo <- enquo(x)
  y_enquo <- enquo(y)
  group_enquo <- enquo(group)

  # rename so that we recycle the workhorse below
  x <- df %>%
    dplyr::rename(x_new = !!x_enquo,
                  y     = !!y_enquo,
                  group = !!group_enquo)

  # message about testing
  message(" * testing on ", length(unique(x$x_new)), " temporal slices")
  message(" * testing pairwise differences using ", substitute(test_fun))

  # now test
  x %>%
    # group by temporal slice and prepare nested tibbles
    dplyr::group_by(x_new) %>%
    tidyr::nest() %>%
    # get the p.value using the appropriate test
    dplyr::mutate(p=purrr::map_dbl(data, test_fun)) %>%
    # we no longer need data
    dplyr::select(-data) %>%
    # nor grouping
    dplyr::ungroup() -> res

  # rename and return
  res %>% dplyr::rename(!!x_enquo := x_new)
}

# Pairwise version. We need a little more preparation to handle each pairwise test
# This is largely delegated to .tidy_pw (private function defined below)
#' @rdname synchrony
#' @export
synchrony_pw <- function(df, x=x_new, y=y, group=group, test_fun=wilcox_p){
  # tidy eval
  x_enquo <- enquo(x)
  y_enquo <- enquo(y)
  group_enquo <- enquo(group)

  # rename so that we recycle the workhorse below
  x <- df %>%
    dplyr::rename(x_new = !!x_enquo,
                  y     = !!y_enquo,
                  group = !!group_enquo)

  # message about testing
  message(" * testing on ", length(unique(x$x_new)), " temporal slices")
  message(" * testing global differences using ", substitute(test_fun))

  # now test
  x %>%
    # group by temporal slice and prepare nested tibbles
    dplyr::group_by(x_new) %>%
    tidyr::nest() %>%
    # prepare these tibbles for pw comparisons then unnest
    dplyr::mutate(data=purrr::map(data, .tidy_pw)) %>%
    tidyr::unnest(data) %>%
    # get the p.value using the appropriate test
    dplyr::mutate(p=purrr::map_dbl(data, test_fun)) %>%
    # we no longer need data
    dplyr::select(-data) %>%
    # nor grouping
    dplyr::ungroup() -> res

  # rename and return
  res %>% dplyr::rename(!!x_enquo := x_new)
}


#' Pairwise comparisons testers
#'
#' These testers return p-value for [synchrony()] pairwise comparisons.
#'
#' @param x [tibble()] with two columns named `group` and `y`,
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
#' x <- tibble::tibble(group=rep(letters[1:2], each=10),
#'                    y=stats::runif(20, -1, 1))
#'
# # only 2 groups
#' wilcox_p(x)
#' aov_p(x)
#'
#' # on >2 groups
#' x$group[15:20] <- "c"
#' kruskal_p(x)
#' aov_p(x)
#' @name synchrony_testers
NULL

#' @describeIn synchrony_testers wilcoxon's rank test for pairwise differences
#' @export
wilcox_p <- function(x){
  # early return if not enough data or levels
  if ((nrow(x)<2) | length(unique(x$group))<2)
    return(NA_real_)
  # otherwise wilcox this and return p.value
  stats::wilcox.test(y~group, data=x)$p.value
}

#' @describeIn synchrony_testers kruskal's test for global differences
#' @export
kruskal_p <- function(x){
  # early return if not enough data or levels
  if ((nrow(x)<2) | length(unique(x$group))<2)
    return(NA_real_)
  # otherwise wilcox this and return p.value
  stats::kruskal.test(y~group, data=x)$p.value
}

#' @describeIn synchrony_testers analysis of variance test for both global and pairwise differences
#' @export
aov_p <- function(x){
  # early return if not enough data or levels
  if ((nrow(x)<2) | length(unique(x$group))<2)
    return(NA_real_)
  # otherwise wilcox this and return p.value
  stats::aov(y~group, data=x) %>%
    # now go get the p_value
    summary() %>% unlist() %>% `[`("Pr(>F)1") %>% as.numeric()
}

# domestic helpers to rearrange pw tibbles
# tidy each permutation
# We have sth like this:
# k      group   y
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
    dplyr::group_by(group) %>%
    dplyr::summarise(y=list(y)) %>%
    # then expand by copying nest df
    tidyr::expand(tidyr::nesting(g1=group, y1=y),
                  tidyr::nesting(g2=group, y2=y)) %>%
    # we then remove i ~ i
    dplyr::filter(g1 != g2) %>%
    # create a sorted paste so that we have our label
    dplyr::mutate(pw=purrr::map2_chr(g1, g2, ~c(.x, .y) %>%
                                       sort %>%
                                       paste0(collapse=" ~ "))) %>%
    # that also serves to only keep 1 in (i ~ j, j ~ i)
    dplyr::distinct(pw, .keep_all = TRUE) %>%
    # there might be a smarter way yet not too bad
    dplyr::mutate(data1=purrr::map2(g1, y1, ~tibble::tibble(group=.x, y=.y)),
                  data2=purrr::map2(g2, y2, ~tibble::tibble(group=.x, y=.y))) %>%
    dplyr::mutate(data=purrr::map2(data1, data2, dplyr::bind_rows)) %>%
    dplyr::select(pw, data)
}
