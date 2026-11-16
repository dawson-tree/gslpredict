#' @export
pclag_predict <- function(year) {
  # Earliest year you can use is 1943? (1946 would be better)

  # Get a year's GSL change
  gsl_change_t1 <- get_gsl_change(year-1)

  # Get a year's prcp
  prcp_t8 <- get_prcp(year-8)
  prcp_t17 <- get_prcp(year-17)
  prcp_t18 <- get_prcp(year-18)
  prcp_t42 <- get_prcp(year-42)

  # Get a year's sst_change
  sst_change_t8 <- get_sst_change(year-8)
  sst_change_t17 <- get_sst_change(year-17)
  sst_change_t18 <- get_sst_change(year-18)
  sst_change_t42 <- get_sst_change(year-42)

  # Get principal components
  pc1_t8 <- pc1_calculate(sst_change_t8, prcp_t8)
  pc1_t17 <- pc1_calculate(sst_change_t17, prcp_t17)
  pc1_t42 <- pc1_calculate(sst_change_t42, prcp_t42)
  pc2_t18 <- pc2_calculate(sst_change_t18, prcp_t18)

  pclag_calculate(gsl_change_t1,
                  pc1_t8,
                  pc1_t17,
                  pc1_t42,
                  pc2_t18)
}
