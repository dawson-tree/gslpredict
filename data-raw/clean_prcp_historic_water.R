library(future)
library(future.apply)
library(parallelly)
library(gslpredict)

n_cores <- availableCores()
prcp_year1s <- 1899:2024
prcp_year2s <- prcp_year1s + 1

# Set up parallel processing
plan(multisession, workers = n_cores - 2)

# Process each year in parallel
clean_prcp <- future_lapply(prcp_years, function(year) {

  result <- tryCatch({
    get_clean_prcp_water(year)
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

names(clean_prcp_historic_water) <- paste0(as.character(prcp_year1s),
                                           "/",
                                           as.character(prcp_year2s))

usethis::use_data(clean_prcp_historic_water, overwrite = TRUE)
