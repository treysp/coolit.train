library(sf)
library(dplyr)
library(raster)
library(coolit.train)

# tower and tile shapefiles
nyc_towers <- st_read("data/source_from-nyc-website/nyc_cooling-tower_shapefile")

nyc_tiles <- st_read("data/source_from-nyc-website/nyc_ortho_jp2/2016 Orthos Tile Index")

nyc_towers <- st_transform(nyc_towers, st_crs(nyc_tiles))

# which tiles have towers?
tile_intersect <- st_intersects(nyc_tiles, nyc_towers)

nyc_tiles$has_tower <- sapply(tile_intersect, function(x) length(x) > 0)

nyc_tiles <- filter(nyc_tiles, has_tower == TRUE)

all_nyc_tiles <- list.files("data/source_from-nyc-website/nyc_ortho_jp2",
                            pattern = "\\.jp2$",
                            full.names = TRUE,
                            recursive = TRUE)

all_nyc_tiles <- data.frame(
  tile_path = all_nyc_tiles,
  IMAGE = stringr::str_match(all_nyc_tiles, "(.*/)*(.*)$")[, 3],
  stringsAsFactors = FALSE
)

nyc_tiles <- left_join(nyc_tiles, all_nyc_tiles, by = "IMAGE")

tiles_to_slice <- nyc_tiles %>%
  filter(!is.na(tile_path)) %>%
  pull(tile_path)

# slice tiles with towers
out_dir <- "data/nyc-64px/nyc-64px-tower-images"

jp2_xml <- xml2::as_list(xml2::read_xml(paste0(tiles_to_slice[1], ".aux.xml")))

my_proj4 <- st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string

nyc_towers <- st_transform(nyc_towers, my_proj4)

cl <- parallel::makeCluster(
  parallel::detectCores() - 1
)

parallel::clusterEvalQ(cl, {
  library(rgdal)
  library(raster)
  library(stringr)
  library(sf)
  library(magick)
  library(dplyr)
})

parallel::clusterExport(cl, c("out_dir", "my_proj4", "nyc_towers"))

results <- parallel::parLapplyLB(cl, tiles_to_slice, function(x) {

  img_stub <- stringr::str_match(x,
                                 "(.*/)*(.*)\\..*$")[, 3]

  temp <- slice_image(
    img_path = x,
    proj4_string = my_proj4,
    slice_n_rows = 64,
    slice_n_cols = 64,
    complete_image = TRUE,
    return_sf = TRUE
    )

  slice_intersect <- st_intersects(temp$geometry, nyc_towers)

  temp$tower_intersect <- sapply(slice_intersect, function(x) length(x) > 0)

  temp <- filter(temp, tower_intersect == TRUE)

  for (i in seq_along(temp$slice_array)) {
    image_write(
      image_read(drop(temp$slice_array[[i]]) / 255),
      paste0(out_dir, "/", img_stub, "_", temp$slice_id[[i]], ".png")
    )
  }
})

parallel::stopCluster(cl)
rm(cl)