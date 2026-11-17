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

# prcp_years <- 1900:2025
#
# clean_prcp <- rep(NA, length(prcp_years))
#
# for (i in seq_along(prcp_years)) {
#   result <- tryCatch({
#     get_clean_prcp(prcp_years[i])
#   }, error = function(e) {
#     message(paste("Error in year ", prcp_years[i], "->", e$message))
#     return(NA) # or return a placeholder / error record
#   })
#
#   clean_prcp[i] <- result
#
#   # Save the result (or error state) to disk immediately
#   saveRDS(clean_prcp,
#           file = "C:/Users/dtree/UCC/Data/saved_clean_prcp.rds")
# }

# clean_prcp_first <- readRDS("C:/Users/dtree/OneDrive/Desktop/UCC/Data/saved_clean_prcp.rds")

# clean_prcp_full <- lapply(1:126, function(i) {
#   year <- i + 1899
#   if (is.null(clean_prcp[[i]])) {
#     tryCatch({
#       prcp <- get_prcp(year)
#       return(prcp)
#     }, error = function(e) {
#       message(paste("Error in year ", x, "->", e$message))
#       return(NULL) # or return a placeholder / error record
#     })
#   } else {
#     return(clean_prcp[[i]])
#   }
# })

clean_prcp_historic_cal <- clean_prcp

names(clean_prcp_historic_cal) <- as.character(1900:2025)

usethis::use_data(clean_prcp_historic_cal, overwrite = TRUE)
