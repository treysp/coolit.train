library(raster)
library(sf)
library(fasterize)

# get image scores
img_scores <- readRDS("C:/Users/wfu3-su/Desktop/tower-id/data/source_from-nyc-website/test/img_980195_scores.rds")
img_scores_df <- img_scores$tile_data
img_scores_df$geometry <- do.call(c, img_scores_df$geometry)
img_scores_df <- st_sf(img_scores_df)

# get source raster
img <- brick("C:/Users/wfu3-su/Desktop/tower-id/data/source_from-nyc-website/test/980195.jp2")
img <- dropLayer(img, 4)
crs(img) <- st_crs(img_scores_df$geometry[[1]])$proj4string

# get NYC tower shapefile
tower_shp <- st_read("C:/Users/wfu3-su/Desktop/tower-id/data/source_from-nyc-website/nyc_cooling-tower_shapefile")
tower_shp <- st_transform(tower_shp, crs = st_crs(img_scores_df$geometry[[1]])$proj4string)
tower_shp_crop <- st_crop(tower_shp, extent(img))

# create df of tiles that overlap known towers
calc_intersect_tiles <- function(tower_shp, tower_scores_df) {
  
  all_intersects_df <- st_intersects(tower_shp, tower_scores_df)
  
  all_intersects_df <- lapply(seq_along(all_intersects_df), function(x) {
    data.frame(
      tower_shp_row = rep(x, length(all_intersects_df[[x]])),
      tower_scores_row = all_intersects_df[[x]]
    )
  })
  
  all_intersects_df <- do.call("rbind", all_intersects_df)
  all_intersects_df$multi_overlap_tile <- duplicated(all_intersects_df$tower_scores_row)
  
  all_intersects_df
}
  

make_tower_overlay <- function(tower_prob = .15, base_raster, tower_scores_df, 
                               tower_shp = NULL, outfilename = NULL) {
# subset scores
tower_scores_df <- tower_scores_df[tower_scores_df$predicted_probs > tower_prob,]
tower_scores_df$geometry <- do.call(c, tower_scores_df$geometry)
tower_scores_df <- st_sf(tower_scores_df)

# rasterize
tower_scores_df$cell_val <- 250
img_scores_raster <- fasterize(tower_scores_df, base_raster[[1]], "cell_val")

out <- base_raster
out[[1]] <- max(out[[1]], img_scores_raster, na.rm = TRUE)

if (!is.null(tower_shp)) {
  tower_shp$cell_val <- 255
  tower_shp_raster <- fasterize(tower_shp, base_raster[[1]], "cell_val")
  
  out[[3]] <- max(out[[3]], tower_shp_raster, na.rm = TRUE)
}

plotRGB(out)

if (!is.null(outfilename)) {
  writeRaster(
    out, 
    outfilename,
    datatype = "INT1U",
    overwrite = TRUE
  )
}

out
}

my_overlay <- make_tower_overlay(
  tower_prob = .07,
  base_raster = img,
  tower_scores_df = img_scores_df,
  tower_shp = tower_shp_crop,
  outfilename = paste0("C:/Users/wfu3-su/Desktop/tower-id/data/source_from-nyc-website/",
                       "test/980195_all-towers_p07.tif")
)
