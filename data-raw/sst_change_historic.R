library(ncdf4)

nc <- nc_open("https://psl.noaa.gov/thredds/dodsC/Datasets/kaplan_sst/sst.mean.anom.nc")

sst <- ncvar_get(nc,
                 "sst",
                 start = c(33, 18, 512),
                 count = c(10, 2, -1))
nc_close(nc)

sst_geo_means <- apply(sst, MARGIN = 3, FUN = mean)

n_years <- length(sst_geo_means) %/% 12
n_months <- n_years * 12

sst_geo_means <- sst_geo_means[1:n_months]

sst_historic <- matrix(sst_geo_means, byrow = FALSE,
                       nrow = 12) |>
  apply(MARGIN = 2, FUN = mean)

sst_change_historic <- vector(mode = "numeric",
                              length = length(sst_historic) - 1)

for (i in 1:(length(sst_historic) - 1)) {
  sst_change_historic[i] <- sst_historic[i+1] -
    sst_historic[i]
}


start_sst_years <- 1898:2021
end_sst_years <- start_sst_years + 1
sst_years <- paste0(as.character(start_sst_years),
                    "/",
                    as.character(end_sst_years))

start_change_years <- sst_years[-length(sst_years)]
end_change_years <- sst_years[-1]
sst_change_years <- paste0(start_change_years,
                           "-",
                           end_change_years)

names(sst_change_historic) <- sst_change_years

usethis::use_data(sst_change_historic, overwrite = TRUE)
