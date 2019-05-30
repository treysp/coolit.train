library(raster)
library(sf)
library(data.table)
library(magick)
library(coolit)

# inspect scores
my_scores <- data.table(
  readRDS("output/2019-05-25/nj-nearmap-scores/nj-nearmap-scores-gt001.rds")
)

my_scores[, pred_prob4 := round(predicted_probs, 4)]

my_scores98 <- my_scores[predicted_probs > .98,]

my_scores98_list <- split(my_scores98, my_scores98$img_id)

cl <- parallel::makeCluster(20)
parallel::clusterEvalQ(cl, {
  library(stringr)
  library(magick)
})

parallel::parLapplyLB(cl = cl, X = my_scores98_list, fun = function(img) {
  img_num <- as.numeric(
    stringr::str_match(unique(img$img_id),
                       "(.*/)*(\\d{1,4})_slices_scores\\.rds")[, 3]
  )

  slices <- readRDS(paste0(
    "f:/wfu3/coolit.train/data/curated-training-slices/nj-nearmap",
    "/nj-nearmap-slices/", img_num, "_slices.rds")
  )

  to_write <- slices[slices$slice_id %in% img$slice_id,]

  for (i in seq_len(nrow(to_write))) {
    image_write(
      image_read(
        drop(to_write$slice_array[[i]]) / 255
      ),
      paste0("c:/users/wfu3/Desktop/temp_nj/", img_num, "_", to_write$slice_id[[i]], ".png")
    )
  }
})

parallel::stopCluster(cl)
rm(cl)
