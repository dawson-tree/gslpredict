# Dynamic stuff

#'@export
plag_predict <- function(year) {
  # Earliest year you can use is 1943? (1946 would be better)

  # Get a year's GSL change
  gsl_change_t1 <- get_gsl_change(year-1)

  # Get a year's prcp
  prcp_t8 <- get_prcp(year-8)
  prcp_t11 <- get_prcp(year-11)
  prcp_t17 <- get_prcp(year-17)
  prcp_t42 <- get_prcp(year-42)


  plag_calculate(gsl_change_t1,
                 prcp_t8,
                 prcp_t11,
                 prcp_t17,
                 prcp_t42)
}

