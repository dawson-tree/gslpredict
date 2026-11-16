library(ncdf4)

nc <- nc_open("https://psl.noaa.gov/thredds/dodsC/Datasets/kaplan_sst/sst.mean.anom.nc")

sst <- ncvar_get(nc,
                 "sst",
                 start = c(33, 18, 524),
                 count = c(10, 2, -1))
nc_close(nc)

sst_geo_means <- apply(sst, MARGIN = 3, FUN = mean)

n_years <- length(sst_geo_means) %/% 12
n_months <- n_years * 12

sst_geo_means <- sst_geo_means[1:n_months]

sst_historic_water <- matrix(sst_geo_means, byrow = FALSE,
                             nrow = 12) |>
  apply(MARGIN = 2, FUN = mean)

start_years <- 1899:2021
end_years <- start_years + 1
sst_years <- paste0(as.character(start_years),
                    "/",
                    as.character(end_years))
names(sst_historic_water) <- sst_years


# All of the yearly SST means for the water years 1899/1900 - 2021/2022

usethis::use_data(sst_historic_water, overwrite = TRUE)
