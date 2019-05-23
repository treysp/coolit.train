library(sf)
library(dplyr)
library(raster)
library(fasterize)

# buildings
# in_path <- "data/source_from-nyc-website/nyc_building-footprint_shapefile"
#
# nyc_building_orig <- st_read(in_path, layer = "building")
# names(nyc_building_orig) <- tolower(names(nyc_building_orig))
#
# nyc_building <- nyc_building_orig %>%
#   filter(feat_code == 2100)
#
# saveRDS(nyc_building, "data/source_from-nyc-website/nyc_building-footprint.rds", compress = FALSE)

nyc_building <- readRDS("data/source_from-nyc-website/nyc_building-footprint.rds")

# identify towers that don't intersect with building polygons
nyc_towers_orig <- st_read("data/source_from-nyc-website/nyc_cooling-tower_shapefile")
nyc_towers <- st_transform(nyc_towers_orig, crs = st_crs(nyc_building))

tower_in_bldg <- st_intersects(nyc_towers, nyc_building)

nyc_towers$bldg_int <- sapply(tower_in_bldg, function(x) length(x) != 0)

nyc_towers_bad <- nyc_towers %>% filter(bldg_int == FALSE)

# nyc jp2 manifest
nyc_tiles <- st_read("data/source_from-nyc-website/nyc_ortho_jp2/2016 Orthos Tile Index")
nyc_tiles <- st_transform(nyc_tiles, crs = st_crs(nyc_building))

nyc_tiles <- st_crop(nyc_tiles, st_bbox(nyc_towers))

nyc_tiles_bad_tower <- st_intersects(nyc_tiles, nyc_towers_bad)

nyc_tiles$has_bad_tower <- sapply(nyc_tiles_bad_tower, function(x) length(x) > 0)

nyc_tiles_bad <- nyc_tiles %>% filter(has_bad_tower == TRUE)

# make overlay of towers on image
make_raster_overlay <- function(base_raster, polygon_sf, outfilename = NULL, write_only = FALSE) {
  if (!is.raster(base_raster) &&
      is.character(base_raster) &&
      length(base_raster) == 1) {

    base_raster_path <- base_raster

    base_raster <- brick(base_raster)
    base_raster <- dropLayer(base_raster, 4)

    jp2_xml <- xml2::as_list(xml2::read_xml(paste0(base_raster_path, ".aux.xml")))

    crs(base_raster) <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string
  }

  # rasterize
  polygon_sf <- lwgeom::st_transform_proj(polygon_sf,
                                          crs = crs(base_raster)@projargs)

  polygon_sf$cell_val <- 250
  overlay_raster <- fasterize(polygon_sf, base_raster[[1]], "cell_val")

  base_raster[[1]] <- overlay(base_raster[[1]],
                              overlay_raster,
                              fun = function(x, y) pmax(x, y, na.rm = TRUE))

  plotRGB(base_raster)

  if (!is.null(outfilename)) {
    writeRaster(
      base_raster,
      outfilename,
      datatype = "INT1U",
      overwrite = TRUE
    )
  }

  if (write_only) {
    return(NULL)
  } else {
    return(base_raster)
  }
}

bad_tower_img <- as.character(unique(nyc_tiles_bad$IMAGE))
jp2_paths <- list.files("data/source_from-nyc-website/nyc_ortho_jp2/",
                        pattern = "\\.jp2$",
                        recursive = TRUE,
                        full.names = TRUE)
bad_tower_paths <- sapply(bad_tower_img, function(x) {
  jp2_paths[grepl(x, jp2_paths)][1]
})

pbapply::pblapply(seq_along(bad_tower_paths), function(i) {
  outfile <- file.path("c:/users/wfu3-su/desktop/temp",
                      paste0(stringr::str_replace(bad_tower_img[i], "\\.jp2", ""), ".tif")
                      )

  make_raster_overlay(base_raster = bad_tower_paths[i],
                      polygon_sf = nyc_towers_bad,
                      outfilename = outfile,
                      write_only = TRUE)

})

# for MS building polygons
test <- data.table::fread("c:/users/wfu3/desktop/NewYork/nyc_microsoft_orig.csv",
                          sep = NULL)
names(test) <- "wkt"
st_as_sf(test[1:5], wkt = "wkt")