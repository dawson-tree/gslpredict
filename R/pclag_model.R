#' @export
pc1_calculate <- function(sst_change,
                          prcp) {
  pc1 <- 0.498*sst_change + 0.867*prcp

  unname(pc1)
}

#' @export
pc2_calculate <- function(sst_change,
                          prcp) {
  pc2 <- 0.867*sst_change - 0.498*prcp

  unname(pc2)
}

#' @export
pclag_calculate <- function(gsl_change_t1,
                            pc1_t8,
                            pc1_t17,
                            pc1_t42,
                            pc2_t18) {
  # browser()
  gsl_change <- 0.508*gsl_change_t1 - 0.614*pc1_t8 -
    0.416*pc1_t17 + 0.442*pc1_t42 +
    0.722*pc2_t18

  unname(gsl_change)
}

#' @export
pclag_parameters <- function(t,
                             gsl_change,
                             pc1,
                             pc2) {

  pclag_calculate(gsl_change[t-1],
                  pc1[t-8],
                  pc1[t-17],
                  pc1[t-42],
                  pc2[t-18])

}

# Now a function to do it for the whole vector

#' #' @export
#' pclag_model <- function(gsl_change,
#'                         prcp,
#'                         sst_change) {
#'   t_min <- min(length(gsl_change),
#'                length(prcp),
#'                length(sst_change))
#'   t_vector <- 43:t_min
#'   subset <- 1:t_min
#'
#'   prcp <- prcp[subset]
#'   sst_change <- sst_change[subset]
#'
#'   pc1 <- pc1_calculate(sst_change, prcp)
#'   pc2 <- pc2_calculate(sst_change, prcp)
#'
#'   results <- lapply(t_vector, function(t) {
#'     pclag_parameters(t,
#'                      gsl_change,
#'                      pc1,
#'                      pc2)
#'   }) |> unlist()
#'   results
#' }


#' @export
pclag_model <- function(gsl_change,
                        prcp,
                        sst_change) {
  t_min <- min(length(prcp),
               length(sst_change))
  t_vector <- 44:length(gsl_change)
  subset <- 1:t_min

  prcp <- prcp[subset]
  sst_change <- sst_change[subset]

  pc1 <- pc1_calculate(sst_change, prcp)
  pc2 <- pc2_calculate(sst_change, prcp)

  results <- lapply(t_vector, function(t) {
    pclag_parameters(t,
                     gsl_change,
                     pc1,
                     pc2)
  }) |> unlist()
  results
}




# gsl_change <- gsl_change_historic
# prcp <- prcp_in_historic
# sst_change <- sst_change_historic
#
# t_min <- min(length(gsl_change),
#              length(prcp),
#              length(sst_change))
# t_vector <- 1:t_min
#
# prcp <- prcp[t_vector]
# sst_change <- sst_change[t_vector]
#
# pc1 <- pc1_calculate(sst_change, prcp)
# pc2 <- pc2_calculate(sst_change, prcp)
#
# results <- lapply(t_vector, function(t) {
#   pclag_parameters(t,
#                    gsl_change,
#                    pc1,
#                    pc2)
# }) |> unlist()
# results
