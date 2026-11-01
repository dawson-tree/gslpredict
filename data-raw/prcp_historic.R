library(readnoaa)
library(sf)
library(dplyr)

# Finds all stations in the watershed bounding box, then spatially filters by
# the actual watershed boundaries

# This data is from https://sgid-utah.opendata.arcgis.com/datasets/utah::utah-huc-boundaries/explore?location=39.672312%2C-110.327773%2C-1
huc <- sf::read_sf("C:/Users/dtree/OneDrive/Desktop/UCC/Data/GslWatershedBoundaries/Utah_HUC_Boundaries_-7067136111917761729/HUC.shp")

gsl_huc <- huc[huc$HUC >= 16010000 & huc$HUC <= 16030000,] |>
  st_transform(crs = 4326)

gsl_bbox <- st_bbox(gsl_huc) |>
  st_transform(crs = 4326)
gsl_bbox <- gsl_bbox[c(2,1,4,3)]

stations <- readnoaa::noaa_stations(bbox = gsl_bbox, limit = 10000L)

stations_sf <- sf::st_as_sf(stations,
                            coords = c("longitude", "latitude"),
                            crs = 4326)

gsl_stations <- st_join(stations_sf,
                        gsl_huc,
                        join = st_intersects,
                        left = FALSE)

gsl_station_ids <- gsl_stations$station

# Note: In the paper, they do only use active ones

prcp_list <- lapply(gsl_station_ids, function (id) {
  noaa_monthly(id, "1900-01", "2026-08",
               datatypes = c("PRCP"),
               units = "standard")
})


prcp_list <- lapply(gsl_station_ids, function (id) {
  tryCatch(
    expr = {
      noaa_monthly(id, "1900-01", "2026-08",
                   datatypes = c("PRCP"),
                   units = "standard")
    },
    error = function(e) {
      return(NA)
    }
  )
})


# Filter out dfs with no rows
valid_stations <- lapply(prcp_list,
                         function(x) {
                           nrow(x) > 0
                         }) |>
  unlist()

valid_prcp_list <- prcp_list[valid_stations]


# Get data ready for use in the model
prcp_df <- do.call("rbind", valid_prcp_list)
# dplyr version sounds like it has better error handling?

valid_prcp_df <- prcp_df[!is.na(prcp_df$prcp),]
valid_prcp_df$prcp <- as.numeric(valid_prcp_df$prcp)


prcp_month_means <- valid_prcp_df |>
  dplyr::group_by(date) |>
  summarize(
    prcp_mean = mean(prcp, na.rm = TRUE)
  )

n_years <- nrow(prcp_month_means) %/% 12
n_months <- n_years * 12

prcp_month_means <- prcp_month_means[1:n_months,]

prcp_historic <- matrix(prcp_month_means$prcp_mean,
                        byrow = FALSE,
                        nrow = 12) |>
  apply(MARGIN = 2, FUN = mean)

names(prcp_historic) <- as.character(1900:2025)

prcp_in_historic <- prcp_historic

usethis::use_data(prcp_in_historic, overwrite = TRUE)

# All of the annual means for precipitation for station in the GSL watershed
# during the calendar years 1900 - 2025

usethis::use_data(prcp_historic, overwrite = TRUE)
