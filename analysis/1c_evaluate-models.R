library(coolit)
library(tensorflow)
library(coolit)
library(keras)
library(ggplot2)
library(magick)
library(ROCR)
library(raster)
library(sf)

# score train/test images
my_model <- load_model_hdf5(
  "output/multi-model-runs/2019-04-09/models/2019-04-09_16-17-24/model_fine-tune-2.h5"
)

temp_scores <- score_test_images(
  "data/tiles_nyc/validation",
  my_model,
  out_filename = "output/multi-model-runs/2019-04-09/models/2019-04-09_16-17-24/predicted-probs_train-validation.csv"
)

temp_scores2 <- score_test_images(
  "data/model-training-data/slices_curated_2019-04-22/train",
  my_model,
  out_filename = "output/multi-model-runs/2019-04-30/models/2019-04-30_17-16-03/predicted-probs_train.csv"
)

temp_scores <- rbind(temp_scores, temp_scores2)

# check some scores
pred <- ROCR::prediction(temp_scores$pred_prob, temp_scores$truth)

message("\nFinal model performance measures:\n",
        "   ROC AUC = ",
        round(performance(pred, "auc")@y.values[[1]], 3), "\n",
        "   Max possible accuracy = ",
        round(max(performance(pred, "acc")@y.values[[1]]), 3)
)

ggplot(temp_scores) +
  geom_histogram(aes(x = pred_prob, y = ..count../sum(..count..),
                     fill = factor(truth)),
                 binwidth = .001, color = NA, alpha = .4) +
  scale_fill_brewer(palette = "Dark2",
                    guide = guide_legend(title = "Truth")) +
  ggtitle("Model predicted probabilities for test set, by actual tower presence") +
  xlab("Predicted probability") +
  ylab("Proportion of all images") +
  theme_minimal()

# examine lowprob towers and higprob non-towers
lowprob_towers <- temp_scores[
  temp_scores$pred_prob < 0.1 & temp_scores$truth == 1,
  ]

lowprob_towers <- lowprob_towers[
  order(-lowprob_towers$pred_prob),
  ]

for (i in 1:((nrow(lowprob_towers) %/% 24) + 1)) {
  if (i == (nrow(lowprob_towers) %/% 24) + 1) {
    index <- nrow(lowprob_towers) - (i - 1) * 24
  } else {
    index <- 24
  }

  image_read(lowprob_towers$img_name[(i - 1) * 24 + 1:index]) %>%
    image_montage() %>%
    image_convert("png") %>%
    image_write(file.path("c:/users/wfu3/desktop/temp",
                          paste0("lowprob_tower_", i, ".png")))
}

highprob_notowers <- temp_scores[
  temp_scores$pred_prob >= 0.1 & temp_scores$truth == 0,
  ]

highprob_notowers <- highprob_notowers[
  order(-highprob_notowers$pred_prob),
  ]

for (i in 1:((nrow(highprob_notowers) %/% 24) + 1)) {
  if (i == (nrow(highprob_notowers) %/% 24) + 1) {
    index <- nrow(highprob_notowers) - (i - 1) * 24
  } else {
    index <- 24
  }

  image_read(highprob_notowers$img_name[(i - 1) * 24 + 1:index]) %>%
    image_montage() %>%
    image_convert("png") %>%
    image_write(file.path("c:/users/wfu3/desktop/temp",
                          paste0("highprob_notower_", i, ".png")))
}

# make raster overlays
overlay_towers <- temp_scores[
  grepl("990212", temp_scores$img_name) &
    temp_scores$pred_prob >= 0.02,
  ]
overlay_towers$tile_id <- as.numeric(stringr::str_match(
  overlay_towers$img_name, "(.*/)\\d*_(\\d*)\\.png")[, 3]
)

overlay_towers_high <- highprob_notowers[
  grepl("990212", highprob_notowers$img_name),
  ]
overlay_towers_high$tile_id <- as.numeric(stringr::str_match(
  overlay_towers_high$img_name, "(.*/)\\d*_(\\d*)\\.png")[, 3]
)

overlay_towers_low <- lowprob_towers[
  grepl("990212", highprob_notowers$img_name),
  ]
overlay_towers_low$tile_id <- as.numeric(stringr::str_match(
  overlay_towers_low$img_name, "(.*/)\\d*_(\\d*)\\.png")[, 3]
  )

overlay_towers_sf <- readRDS("output/2019-03-31/sliced_nyc/990212_slices.rds")
overlay_towers_sf$geometry <- st_sfc(do.call("c", overlay_towers_sf$geometry))
overlay_towers_sf <- st_sf(overlay_towers_sf)

base_raster_path <- "data/source_from-nyc-website/nyc_ortho_jp2/boro_manhattan_sp16/990212.jp2"
base_raster <- brick(base_raster_path)
base_raster <- dropLayer(base_raster, 4)

jp2_xml <- xml2::as_list(xml2::read_xml(paste0(base_raster_path, ".aux.xml")))

crs(base_raster) <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string

nyc_towers <- st_read("data/source_from-nyc-website/nyc_cooling-tower_shapefile")

nyc_towers <- lwgeom::st_transform_proj(nyc_towers,
                                        crs = crs(base_raster)@projargs)

nyc_towers <- st_crop(nyc_towers, st_bbox(base_raster))
nyc_towers_buffer <- st_buffer(nyc_towers, 20)

make_tower_plot <- function(tower_raster, scores_sf = NULL,
                            red_tile_ids = NULL, green_tile_ids = NULL,
                            orange_tile_ids = NULL, tower_sf = NULL, outpath) {
  png(outpath,
      width = ncol(tower_raster), height = nrow(tower_raster))

  on.exit(dev.off())

  plotRGB(tower_raster, maxpixels = 1e10)

  if (!is.null(red_tile_ids)) {
    plot(
      scores_sf$geometry[scores_sf$tile_id %in% red_tile_ids],
      col = scales::alpha("red", .4),
      add = TRUE
    )
  }

  if (!is.null(green_tile_ids)) {
    plot(
      scores_sf$geometry[scores_sf$tile_id %in% green_tile_ids],
      col = scales::alpha("green", .4),
      add = TRUE
    )
  }

  if (!is.null(orange_tile_ids)) {
    plot(
      scores_sf$geometry[scores_sf$tile_id %in% orange_tile_ids],
      col = scales::alpha("orange", .4),
      add = TRUE
    )
  }

  if (!is.null(tower_sf)) {
    plot(
      tower_sf,
      col = scales::alpha("blue", .3),
      add = TRUE
    )
  }
}

make_tower_plot(base_raster,
                scores_sf = overlay_towers_sf,
                red_tile_ids = overlay_towers$tile_id,
                green_tile_ids = overlay_towers_high$tile_id,
                orange_tile_ids = overlay_towers_low$tile_id,
                tower_sf = nyc_towers_buffer$geometry,
                outpath = "c:/users/wfu3/desktop/test.png")
