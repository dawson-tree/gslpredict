#' @export
plag_calculate <- function(gsl_change_t1,
                           prcp_t8,
                           prcp_t11,
                           prcp_t17,
                           prcp_t42) {
  gsl_change <- 0.619*gsl_change_t1 - 0.411*prcp_t8 +
    0.386*prcp_t11 - 0.399*prcp_t17 +
    0.425*prcp_t42

  unname(gsl_change)
}

#' @export
plag_parameters <- function(t,
                            gsl_change,
                            prcp) {

  plag_calculate(gsl_change[t-1],
                 prcp[t-8],
                 prcp[t-11],
                 prcp[t-17],
                 prcp[t-42])

}

# Now a function to do it for the whole vector

#' @export
plag_model <- function(gsl_change,
                       prcp) {
  t_min <- min(length(gsl_change), length(prcp))
  t_vector <- 44:t_min
  results <- lapply(t_vector, function(t) {
    plag_parameters(t,
                    gsl_change,
                    prcp)
  }) |> unlist()
  results
}
