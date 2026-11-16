# Gets the GSL change for one year
# library(dataRetrieval)
# library(dplyr)

#' @importFrom dataRetrieval read_waterdata_daily
#' @importFrom dplyr group_by summarize
#'
#' @export
get_gsl_change <- function(year) {
  start_year <- as.character(year-1)
  end_year <- as.character(year)

  gsl_elev <- dataRetrieval::read_waterdata_daily(
    monitoring_location_id = "USGS-10010000",
    time = c(paste0(start_year, "-01-01"),
             paste0(end_year, "-12-31"))
  )

  years <- as.numeric(start_year):as.numeric(end_year)
  gsl_elev$year <- format(gsl_elev$time, "%Y")

  year_means <- gsl_elev |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      elev_mean = mean(value, na.rm = TRUE)
  )

  # Change from the previous year in annual elevation mean for the south arm of
  # the Great Salt Lake (Station USGS-10010000)

  year_means$elev_mean[2] - year_means$elev_mean[1]

}


# Gets the average GSL watershed precipitation for one year
# library(readnoaa)
# library(sf)
# library(dplyr)
# year argument indicates the larger calendar year that is part of the water
# year

#' #' @importFrom readnoaa noaa_stations noaa_monthly
#' #' @importFrom sf st_bbox st_transform st_as_sf st_join
#' #' @importFrom dplyr group_by summarize
#' #'
#' #' @export
#'
#' get_prcp <- function(year) {
#'
#'   # Finds all stations in the watershed bounding box, then spatially filters by
#'   # the actual watershed boundaries
#'
#'   gsl_bbox <- sf::st_bbox(gsl_watershed) |>
#'     sf::st_transform(crs = 4326)
#'   gsl_bbox <- gsl_bbox[c(2,1,4,3)]
#'
#'   stations <- readnoaa::noaa_stations(bbox = gsl_bbox, limit = 10000L)
#'
#'   stations_sf <- sf::st_as_sf(stations,
#'                               coords = c("longitude", "latitude"),
#'                               crs = 4326)
#'
#'   gsl_stations <- sf::st_join(stations_sf,
#'                               gsl_watershed,
#'                               join = st_intersects,
#'                               left = FALSE)
#'
#'   gsl_station_ids <- gsl_stations$station
#'
#'   # Note: In the paper, they only use active ones, we use all available stations
#'   # here
#'
#'   year <- as.character(year)
#'
#'   prcp_list <- lapply(gsl_station_ids, function (id) {
#'     readnoaa::noaa_monthly(id,
#'                            paste0(year, "-01"),
#'                            paste0(year, "-12"),
#'                            datatypes = c("PRCP"),
#'                            units = "standard")
#'   })
#'
#'
#'   # Filter out dfs with no rows
#'   valid_stations <- lapply(prcp_list,
#'                            function(x) {
#'                              nrow(x) > 0
#'                            }) |>
#'     unlist()
#'
#'   valid_prcp_list <- prcp_list[valid_stations]
#'
#'
#'   # Get data ready for use in the model
#'   prcp_df <- do.call("rbind", valid_prcp_list)
#'   # dplyr version sounds like it has better error handling?
#'
#'   valid_prcp_df <- prcp_df[!is.na(prcp_df$prcp),]
#'   valid_prcp_df$prcp <- as.numeric(valid_prcp_df$prcp)
#'
#'
#'   prcp_month_means <- valid_prcp_df |>
#'     dplyr::group_by(date) |>
#'     dplyr::summarize(
#'       prcp_mean = mean(prcp, na.rm = TRUE)
#'     )
#'
#'   mean(prcp_month_means$prcp_mean)
#'
#' }

# library(ncdf4)
#' @importFrom ncdf4 nc_open ncvar_get nc_close
get_sst_change <- function(year) {

  time_start <- (year - 1857)*12 - 4

  nc <- ncdf4::nc_open(paste0("https://psl.noaa.gov/thredds/dodsC/Datasets/",
                              "kaplan_sst/sst.mean.anom.nc"))

  sst <- ncdf4::ncvar_get(nc,
                   "sst",
                   start = c(33, 18, time_start),
                   count = c(10, 2, 24))
  ncdf4::nc_close(nc)

  sst_geo_means <- apply(sst, MARGIN = 3, FUN = mean)

  sst_year_means <- matrix(sst_geo_means,
                           byrow = FALSE,
                           nrow = 12) |>
    apply(MARGIN = 2, FUN = mean)

  sst_year_means[2] - sst_year_means[1]
}


# This is way faster for one year stuff, but would be much slower for multi-year stuff

# library(data.table)
# library(dplyr)
# library(sf)
# # Need R.utils

#' @importFrom readnoaa noaa_stations
#' @importFrom sf st_bbox st_transform st_as_sf st_join st_intersects
#' @importFrom dplyr group_by summarize n
#' @importFrom data.table fread
#'
#' @export
get_prcp <- function(year) {

  url <- paste0("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year/",
                year,
                ".csv.gz")

  old_opts <- options(timeout = 1000)
  on.exit(options(old_opts))
  year_record <- data.table::fread(url,
                                  data.table = FALSE)


  year_prcp <- year_record[year_record$V3 == "PRCP",]

  gsl_bbox <- sf::st_bbox(gsl_watershed) |>
    sf::st_transform(crs = 4326)
  gsl_bbox <- gsl_bbox[c(2,1,4,3)]

  stations <- readnoaa::noaa_stations(bbox = gsl_bbox, limit = 10000L)
  stations_sf <- sf::st_as_sf(stations,
                              coords = c("longitude", "latitude"),
                              crs = 4326)
  gsl_stations <- sf::st_join(stations_sf,
                              gsl_watershed,
                              join = sf::st_intersects,
                              left = FALSE)
  gsl_station_ids <- gsl_stations$station


  gsl_year_prcp <- year_prcp[year_prcp$V1 %in% gsl_station_ids,]

  gsl_year_prcp$month <- gsl_year_prcp$V2 |>
    as.character() |>
    substr(start = 1, stop = 6)

  gsl_year_prcp$year <- gsl_year_prcp$V2 |>
    as.character() |>
    substr(start = 1, stop = 4)

  prcp_month_means <- gsl_year_prcp |>
    dplyr::group_by(V1, year, month) |>
    dplyr::summarize(
      prcp_month_mean = mean(V4, na.rm = TRUE),
      prcp_month_n = dplyr::n()
    )

  # Only include months with at least 25 days of data
  prcp_month_means <- prcp_month_means[prcp_month_means$prcp_month_n >= 25,]

  prcp_year_means <- prcp_month_means |>
    dplyr::group_by(V1, year) |>
    dplyr::summarize(
      prcp_year_mean = mean(prcp_month_mean, na.rm = TRUE),
      prcp_year_n = dplyr::n()
    )

  # Only include stations with valid data for every month of the year
  prcp_year_means <- prcp_year_means[prcp_year_means$prcp_year_n == 12,]

  # Find the mean and convert it from mm to inches
  mean(prcp_year_means$prcp_year_mean)/25.4

}


#' @importFrom readnoaa noaa_stations
#' @importFrom sf st_bbox st_transform st_as_sf st_join st_intersects
#' @importFrom dplyr group_by summarize n
#' @importFrom data.table fread
#'
#' @export
get_clean_prcp <- function(year) {

  url <- paste0("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year/",
                year,
                ".csv.gz")

  old_opts <- options(timeout = 1000)
  on.exit(options(old_opts))
  year_record <- data.table::fread(url,
                                   data.table = FALSE)


  year_prcp <- year_record[year_record$V3 == "PRCP",]

  gsl_bbox <- sf::st_bbox(gsl_watershed) |>
    sf::st_transform(crs = 4326)
  gsl_bbox <- gsl_bbox[c(2,1,4,3)]

  stations <- readnoaa::noaa_stations(bbox = gsl_bbox, limit = 10000L)
  stations_sf <- sf::st_as_sf(stations,
                              coords = c("longitude", "latitude"),
                              crs = 4326)
  gsl_stations <- sf::st_join(stations_sf,
                              gsl_watershed,
                              join = sf::st_intersects,
                              left = FALSE)
  gsl_station_ids <- gsl_stations$station

  gsl_year_prcp <- year_prcp[year_prcp$V1 %in% gsl_station_ids,]

  gsl_year_prcp$year <- gsl_year_prcp$V2 |>
    as.character() |>
    substr(start = 1, stop = 4)

  gsl_year_prcp$month <- gsl_year_prcp$V2 |>
    as.character() |>
    substr(start = 5, stop = 6)

  prcp_month_means <- gsl_year_prcp |>
    dplyr::group_by(V1, year, month) |>
    dplyr::summarize(
      prcp_month_mean = mean(V4, na.rm = TRUE),
      prcp_month_n = dplyr::n()
    )

  # Only include months with at least 90% non-missing days
  prcp_month_means$cutoff <- apply(prcp_month_means, MARGIN = 1, function(data) {
    (0.9 * month_n_days(data['month'],
                        data['year'])) |>
      ceiling()
  })

  prcp_month_means <- prcp_month_means[
    prcp_month_means$prcp_month_n >= prcp_month_means$cutoff,
  ]

  prcp_year_means <- prcp_month_means |>
    dplyr::group_by(V1, year) |>
    dplyr::summarize(
      prcp_year_mean = mean(prcp_month_mean, na.rm = TRUE),
      prcp_year_n = dplyr::n()
    )

  # Only include stations with valid data for every month of the year
  prcp_year_means <- prcp_year_means[prcp_year_means$prcp_year_n == 12,]

  # Find the mean and convert it from mm to inches
  mean(prcp_year_means$prcp_year_mean)/25.4

}


month_n_days <- function(month, year) {
  first_day_string <- paste0(year, "-", month, "-01")
  first_day <- as.Date(format(as.Date(first_day_string), "%Y-%m-01"))

  # Find the 1st day of the following month
  next_month <- seq(first_day, length = 2, by = "1 month")[2]

  # Subtract the two dates to get the difference in days
  as.integer(next_month - first_day)
}



