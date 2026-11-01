cleaned_prcp <- lapply(1900:2025, function(x) {
  result <- tryCatch({
    get_prcp(x)
  }, error = function(e) {
    message(paste("Error in year ", x, "->", e$message))
    return(NULL) # or return a placeholder / error record
  })

  # Save the result (or error state) to disk immediately
  file_path <- paste0("C:/Users/dtree/OneDrive/Desktop/UCC/Data/saved_prcp_",
                      x,
                      ".rds")
  saveRDS(result, file = file_path)

  return(result)

}
)

cleaned_prcp_full <- lapply(1:126, function(i) {
  year <- i + 1899
  if (is.null(cleaned_prcp[[i]])) {
    tryCatch({
      prcp <- get_prcp(year)
      return(prcp)
    }, error = function(e) {
      message(paste("Error in year ", x, "->", e$message))
      return(NULL) # or return a placeholder / error record
    })
  } else {
    return(cleaned_prcp[[i]])
  }
})

cleaned_prcp_historic <- unlist(cleaned_prcp_full)

names(cleaned_prcp_historic) <- as.character(1900:2025)

usethis::use_data(cleaned_prcp_historic, overwrite = TRUE)
