library(future)
library(future.apply)
library(parallelly)
library(gslpredict)

n_cores <- availableCores()
prcp_years <- 1900:2025

# Set up parallel processing
plan(multisession, workers = n_cores - 2)

# Process each year in parallel
clean_prcp <- future_lapply(prcp_years, function(year) {

  result <- tryCatch({
    get_clean_prcp_cal(year)
  }, error = function(e) {
    message(paste("Error in year", year, "->", e$message))
    return(NA)
  })

  return(result)
})

# Convert list to vector if appropriate
clean_prcp <- unlist(clean_prcp)

# Save results
saveRDS(
  clean_prcp,
  file = "C:/Users/A02324772/Desktop/UCC Data/saved_clean_prcp_cal.rds"
)

# Shut down parallel workers
plan(sequential)



clean_prcp_historic_water <- clean_prcp

names(clean_prcp_historic_water) <- as.character(1900:2025)

usethis::use_data(clean_prcp_historic_water, overwrite = TRUE)
