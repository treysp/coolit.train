library(raster)
library(sf)
library(magick)
library(coolit)

# get chi img_projection
chi_json <- readRDS("data/source_from-chi-website/json-info/chi_json-info_2019-05-02.rds")

my_proj4 <- st_crs(wkt = chi_json[[1]]$extent$spatialReference$wkt)$proj4string

# get cooling tower coords
tower_coord <- readxl::read_excel("data/source_from-chi-website/tower-coords/Chicago_CT_ALL.xlsx")
names(tower_coord) <- tolower(names(tower_coord))

tower_coord <- st_as_sf(tower_coord,
                        coords = c("point_x", "point_y"),
                        agr = "constant",
                        crs = 4326)

tower_coord$geometry <- st_transform(tower_coord$geometry, crs = my_proj4)

# save png slices that intersect with tower coord + 50ft buffer
coord_buffered <- st_buffer(tower_coord$geometry, 50)

cl <- parallel::makeCluster(parallel::detectCores() - 1)

parallel::clusterEvalQ(cl, {
  library(raster)
  library(sf)
  library(magick)
})

parallel::parLapplyLB(X = 1:5573, cl = cl, fun = function(i) {

  temp_slices <- readRDS(
    paste0("data/source_from-chi-website/chi-slices/",
           i,
           "_slices.rds"
           )
  )

  temp_slices$tower_intersect <- sapply(
    st_intersects(temp_slices, coord_buffered),
    function(x) length(x) > 0)

  temp_slices <- temp_slices[temp_slices$tower_intersect == TRUE,]

  for (j in seq_len(nrow(temp_slices))) {
    magick::image_write(
      image = image_read(drop(temp_slices[["slice_array"]][j][[1]]) / 255),
      path = file.path("c:/users/wfu3/desktop/temp",
                       paste0(i, "_", temp_slices[["slice_id"]][j], ".png")))
  }
})

parallel::stopCluster(cl)
