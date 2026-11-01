library(sf)

# This data is from https://sgid-utah.opendata.arcgis.com/datasets/utah::utah-huc-boundaries/explore?location=39.672312%2C-110.327773%2C-1
huc <- sf::read_sf("C:/Users/dtree/OneDrive/Desktop/UCC/Data/GslWatershedBoundaries/Utah_HUC_Boundaries_-7067136111917761729/HUC.shp")

gsl_watershed <- huc[huc$HUC >= 16010000 & huc$HUC <= 16030000,] |>
  st_transform(crs = 4326)

usethis::use_data(gsl_watershed, overwrite = TRUE)
