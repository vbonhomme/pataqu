% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{animals_q}
\alias{animals_q}
\title{Animals with 20 permutations on 7 temporal slices}
\format{
An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 31700 rows and 8 columns.
}
\usage{
animals_q
}
\description{
See \link{animals}. This is obtained with:\preformatted{set.seed(2329)
animals_q <-  animals \%>\%
  dplyr::filter(tpq>-100, taq<100) \%>\%
  quake(k=20, shaker=shake_uniform, tpq, taq)
usethis::use_data(animals_q, overwrite=TRUE)
}
}
\keyword{datasets}
