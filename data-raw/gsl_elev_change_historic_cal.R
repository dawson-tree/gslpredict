library(dataRetrieval)
library(dplyr)

gsl_elev <- read_waterdata_daily(monitoring_location_id = "USGS-10010000",
                                 time = c("1899-01-01", "2025-12-31"))

years <- 1899:2025
gsl_elev$year <- format(gsl_elev$time, "%Y")

gsl_elev_year_means <- gsl_elev |>
  dplyr::group_by(year) |>
  summarize(
    elev_mean = mean(value, na.rm = TRUE)
  )

gsl_change_historic_cal <- vector(mode = "numeric",
                              length = nrow(gsl_elev_year_means) - 1)

for (i in 1:(nrow(gsl_elev_year_means) - 1)) {
  gsl_change_historic_cal[i] <- gsl_elev_year_means$elev_mean[i+1] -
    gsl_elev_year_means$elev_mean[i]
}

start_years <- years[-length(years)]
end_years <- start_years + 1
gsl_change_years <- paste0(as.character(start_years),
                           "-",
                           as.character(end_years))
names(gsl_change_historic_cal) <- gsl_change_years



# All of the year-to-year changes in annual elevation mean for the south arm of
# the Great Salt Lake (Station USGS-10010000) for the calendar years
# 1900 - 2025

usethis::use_data(gsl_change_historic_cal, overwrite = TRUE)
