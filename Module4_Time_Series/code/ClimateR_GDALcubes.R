##Script to demo pulling climate and RS time-series.

##Author: Ian Breckheimer
##Updated: 1-29-2024


##Installing from Github
#remotes::install_github("mikejohnson51/AOI")
#remotes::install_github("mikejohnson51/climateR")

## Demo script for climateR and gdalcubes
library(sf)
library(rstac)
library(AOI)
library(climateR)
library(tidyr)
library(ggplot2)
library(gdalcubes)
library(terra)

##Gets Gunnison County AOI
gc_bound <- aoi_get(state="Colorado",county="Gunnison")
gc_bound

##Gets PRISM data for Gunnison County.
gridmet_prcp <- getGridMET(AOI = gc_bound,
                  varname = "pr",
                  startDate = "1991-10-29",
                  endDate  = "1991-11-06")

plot(gridmet_prcp[[1]][[1]])
plot(gc_bound,add=TRUE)

##Creates random points in bounding box.
gc_pts <- st_sample(gc_bound,10)
plot(gc_bound$geometry)
plot(gc_pts,add=TRUE)

##Extracts data from points.
gc_prcp <- terra::extract(gridmet_prcp[[1]],vect(gc_pts))
gc_prcp_long <- pivot_longer(gc_prcp,cols=2:10,names_to="date",values_to="prcp")

##Quick plot.
ggplot(gc_prcp_long)+
  geom_point(aes(x=date,y=prcp, color=as.factor(ID)))+
  theme_bw()

##Getting Sentinel 2 time-series using STAC and gdalcubes.
stac("https://earth-search.aws.element84.com/v1") |>
  get_request()

collection_formats()


s = stac("https://earth-search.aws.element84.com/v0")

##Search for Sentinel 2 data.
# Search for Sentinel-2 images within specified bounding box and date range
#22 Million items

# bounding box of Gunnison County
gc_bbox <- st_bbox(gc_bound)

gc_bbox_32720 <- gc_bound |> 
                 st_transform(crs =32720 ) |>
                 st_bbox() 

items = s |>
  stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox = c(gc_bbox["xmin"], 
                       gc_bbox["ymin"],
                       gc_bbox["xmax"], 
                       gc_bbox["ymax"]), 
              datetime = "2021-05-15/2021-05-16") |>
  post_request() |>
  items_fetch(progress = FALSE)
length(items$features)

# Prepare the assets for analysis
library(gdalcubes)
assets = c("B01", "B02", "B03", "B04", "B05", "B06", 
           "B07", 
           "B08", "B8A", "B09", "B11", "B12", "SCL")
s2_collection = stac_image_collection(items$features, asset_names = assets,
                                      property_filter = function(x) {x[["eo:cloud_cover"]] < 20}) #all images with less than 20% clouds
s2_collection

# Define a specific view on the satellite image collection
v = cube_view(
  srs = "EPSG:32720", #this is harder than expected. 
  dx = 100, 
  dy = 100, 
  dt = "P10D", 
  aggregation = "median", 
  resampling = "near",
  extent = list(
    t0 = "2021-05-15", 
    t1 = "2021-07-16",
    left = gc_bbox_32720[1], 
    right = gc_bbox_32720[3],
    top = gc_bbox_32720[4], 
    bottom = gc_bbox_32720[2]
  )
)

# Take a "picture"
gdalcubes_options(parallel = 4)
gc_s2_ndvi <- s2_collection |>
                raster_cube(v) |>
                select_bands(c( "B04", "B05"))  |>
                apply_pixel(c("(B05-B04)/(B05+B04)"), names="NDVI") |>
                write_tif() |>
                terra::rast()
plot(gc_s2_ndvi)
