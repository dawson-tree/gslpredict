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

gsl_elev_historic <- gsl_elev_year_means$elev_mean
names(gsl_elev_historic) <- as.character(years)


# All of the annual elevation means for the south arm of the Great Salt Lake
# (Station USGS-10010000) during the calendar years 1899 - 2025

usethis::use_data(gsl_elev_historic, overwrite = TRUE)
