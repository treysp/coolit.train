library(sf)
library(igraph)
library(dplyr)

tower_slices <- list.files("c:/users/wfu3/desktop/temp_nj_2019-05-28/tower")

tower_slices <- data.frame(
  img_id = stringr::str_match(tower_slices, "(\\d{1,4})_(\\d{1,4})\\.png")[, 2],
  slice_id = stringr::str_match(tower_slices, "(\\d{1,4})_(\\d{1,4})\\.png")[, 3]
)

tower_slice_list <- split(tower_slices, tower_slices$img_id)

cl <- parallel::makeCluster(20)

my_tower_slices <- pbapply::pblapply(cl = cl,
                                     X = tower_slice_list,
                                     FUN = function(img) {
  slices <- readRDS(paste0(
    "f:/wfu3/coolit.train/data/curated-training-slices/nj-nearmap",
    "/nj-nearmap-slices/", unique(img$img_id), "_slices.rds")
  )

  slices$img_id <- unique(img$img_id)

  slices[slices$slice_id %in% img$slice_id,]
})

parallel::stopCluster(cl)
rm(cl)

my_tower_slices <- do.call("rbind", my_tower_slices)

my_tower_slices$geometry <- do.call("c", my_tower_slices$geometry)

my_tower_slices <- st_sf(my_tower_slices)

my_tower_centroids <- dissolve_slices(
  my_tower_slices,
  out_geometry = "centroid",
  out_name = "output/2019-05-28/nj-nearmap_tower-points_2019-06-06.kml"
  )

zip("output/2019-05-28/nj-nearmap_tower-points_2019-06-06.kmz",
    "output/2019-05-28/nj-nearmap_tower-points_2019-06-06.kml")