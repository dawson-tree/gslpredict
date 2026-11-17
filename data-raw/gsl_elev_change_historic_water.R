library(dataRetrieval)
library(dplyr)

gsl_elev <- read_waterdata_daily(monitoring_location_id = "USGS-10010000",
                                 time = c("1898-08-01", "2025-07-31"))

start_years <- 1898:2024
end_years <- start_years + 1

gsl_elev$year_mo <- format(gsl_elev$time, "%Y%m")
gsl_elev$year <- format(gsl_elev$time, "%Y") |>
  as.numeric()
gsl_elev$month <- format(gsl_elev$time, "%m") |>
  as.numeric()


gsl_elev$water_year <- ifelse(gsl_elev$month > 7,
                              paste0(as.character(gsl_elev$year),
                                     "/",
                                     as.character(gsl_elev$year + 1)),
                              paste0(as.character(gsl_elev$year - 1),
                                     "/",
                                     as.character(gsl_elev$year))
)

gsl_elev_year_means <- gsl_elev |>
  dplyr::group_by(water_year) |>
  summarize(
    elev_mean = mean(value, na.rm = TRUE)
  )

gsl_elev_change_historic_water <- vector(mode = "numeric",
                                  length = nrow(gsl_elev_year_means) - 1)

for (i in 1:(nrow(gsl_elev_year_means) - 1)) {
  gsl_elev_change_historic_water[i] <- gsl_elev_year_means$elev_mean[i+1] -
    gsl_elev_year_means$elev_mean[i]
}


gsl_water_years <- paste0(as.character(start_years),
                           "/",
                           as.character(end_years))

start_water_years <- gsl_water_years[-length(gsl_water_years)]
end_water_years <- gsl_water_years[-1]
gsl_change_water_years <- paste0(as.character(start_water_years),
                           "-",
                           as.character(end_water_years))
names(gsl_elev_change_historic_water) <- gsl_change_water_years



# All of the year-to-year changes in annual elevation mean for the south arm of
# the Great Salt Lake (Station USGS-10010000) for the calendar years
# 1900 - 2025

usethis::use_data(gsl_elev_change_historic_water, overwrite = TRUE)
