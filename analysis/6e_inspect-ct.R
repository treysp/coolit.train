library(raster)
library(sf)
library(data.table)
library(magick)
library(coolit)

# inspect scores
my_scores <- data.table(
  readRDS("output/2019-07-19/ct-scores/ct-scores_gt001_2019-07-19.rds")
)

my_scores[, pred_prob4 := round(predicted_probs, 4)]

my_scores5 <- my_scores[predicted_probs > .9,]

my_scores5_list <- split(my_scores5, my_scores5$img_id)

cl <- parallel::makeCluster(30)
parallel::clusterEvalQ(cl, {
  library(stringr)
  library(magick)
})

parallel::parLapplyLB(cl = cl, X = my_scores5_list, fun = function(img) {
  img_num <- stringr::str_match(unique(img$img_id),
                       "(.*/)*(.*)_slices_scores\\.rds")[, 3]

  slices <- readRDS(paste0(
    "data/curated-training-slices/ct",
    "/ct-slices/", img_num, "_slices.rds")
  )

  to_write <- slices[slices$slice_id %in% img$slice_id,]

  for (i in seq_len(nrow(to_write))) {
    image_write(
      image_read(
        drop(to_write$slice_array[[i]]) / 255
      ),
      paste0("output/2019-07-19/ct-images/",
             img_num, "_", to_write$slice_id[[i]], ".png")
    )
  }
})

parallel::stopCluster(cl)
rm(cl)
