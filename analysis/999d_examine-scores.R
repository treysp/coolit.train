library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)
library(magick)

# # compile tile scores and save
# score_dir <- "F:/coolit/output/2019-03-31/scored_nyc_pre-sliced"
# scores <- pbapply::pblapply(list.files(score_dir, full.names = TRUE), function(x) {
#   out <- readRDS(x)
#   out <- out[["tile_data"]]
#
#   out$source_img_aux <- NULL
#   out$tile_id <- NULL
#   out$x0 <- NULL
#   out$x1 <- NULL
#   out$y0 <- NULL
#   out$y1 <- NULL
#   out$extents <- NULL
#
#   out
#   })
#
# scores_df <- bind_rows(scores) %>%
#   mutate(
#     unique_id = paste0(
#       stringr::str_match(source_img, "(.*/)*(.*?)\\.jp2")[, 3],
#       "_",
#       tile_id
#       ),
#
#     row_num = row_number()
#     )
#
# scores_df$geometry <- do.call(c, scores_df$geometry)
# scores_df <- st_sf(scores_df)
#
# saveRDS(scores_df, file.path(score_dir, "collated_scores.rds"))
# saveRDS(scores_df[scores_df$predicted_probs >= .001, ],
#         file.path(score_dir, "collated_scores001.rds"))

score_dir <- "output/2019-03-31/scored_nyc"
scores_df <- readRDS(file.path(score_dir, "collated_scores001.rds"))

# get NYC tower shapefile
tower_shp <- st_read("data/source_from-nyc-website/nyc_cooling-tower_shapefile")
tower_shp <- st_transform(tower_shp, crs = st_crs(scores_df$geometry)$proj4string)
tower_shp$num_id <- seq_len(nrow(tower_shp))

score_tile_intersect <- st_intersects(scores_df, tower_shp)
scores_df$tower_intersect <- sapply(score_tile_intersect,
                                    function(x) if (length(x) == 0) FALSE else TRUE)

scores_dt <- data.table(scores_df)
scores_dt[, pred_prob01 := round(predicted_probs, 2)]

my_means <- scores_dt[, list(
  mn_tower_intersect = mean(tower_intersect),
  ct_tower_intersect = sum(tower_intersect),
  ct_all = sum(!is.na(tower_intersect))
  ), by = pred_prob01]

ggplot(data = my_means) +
  geom_line(aes(x = pred_prob01, y = mn_tower_intersect)) +
  xlab("Predicted prob. of tower") +
  ylab("Prop. of tiles with tower") +
  theme_minimal()

# save poorly scored slices to file
bad_towers <- scores_dt[pred_prob01 < 0.25 &
                          tower_intersect == 1]
bad_towers <- do.call("rbind", strsplit(bad_towers$unique_id, "_"))
bad_towers <- data.frame(img_id = bad_towers[,1],
                         row_number = as.numeric(bad_towers[,2]),
                         stringsAsFactors = FALSE)

# pbapply::pblapply(split(bad_towers, bad_towers$img_id), function(x) {
#   img_id <- unique(x$img_id)
#
#   img_df <- readRDS(file.path("output/2019-03-31/sliced_nyc",
#                               paste0(img_id, "_slices.rds")))
#
#   img_df <- img_df[img_df$tile_id %in% x$row_number,]
#
#   for (i in seq_len(nrow(img_df))) {
#     magick::image_write(
#       image = image_read(drop(img_df[i, "tile_array"][[1]]) / 255),
#       path = file.path("c:/users/wfu3-su/Desktop/temp/bad-tower-slices",
#                        paste0(img_id, "_", img_df[i, "tile_id"], ".png")))
#   }
#
# })

# identify jp2 images with any towers
img_index <- st_read("data/source_from-nyc-website/nyc_ortho_jp2/2016 Orthos Tile Index")
img_index <- st_transform(img_index, crs = st_crs(tower_shp))

img_tower_int <- st_intersects(img_index, tower_shp)
img_index$has_tower <- sapply(img_tower_int, function(x) length(x) > 0)

saveRDS(img_index[img_index$has_tower,],
        "output/2019-04-06/img-index_has-towers_2019-04-08.rds")
