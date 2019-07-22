library(keras)
library(raster)
library(coolit.train)
library(data.table)
library(sf)

# score slices
scores <- score_slice_data_dir(
  slice_data_dir = "data/curated-training-slices/ct/ct-slices/zips-to-score",
  model_params_dput_file = "output/multi-model-runs/2019-07-02/models/2019-07-02_11-08-43/run-parameters_dput.txt",
  model_h5_weights = "output/multi-model-runs/2019-07-02/models/2019-07-02_11-08-43/model_fine-tune-2.h5",
  score_outdir = "output/2019-07-19/ct-scores",
  return_score = FALSE
)

my_scores <- lapply(list.files("output/2019-07-19/ct-scores",
                               full.names = TRUE),
                    function(x) {
  temp <- readRDS(x)
  temp <- data.table(temp[["slice_data"]])

  temp$img_id <- x

  tryCatch({
    temp$geometry <- do.call("c", temp$geometry)
  },
  error = function(e) browser())


  list(
    lte001 = temp[predicted_probs <= 0.001],
    gt001 = temp[predicted_probs > 0.001]
  )
})

my_scores <- purrr::transpose(my_scores)

lte_001 <- rbindlist(my_scores$lte001)
gt_001 <- rbindlist(my_scores$gt001)
rm(my_scores)

saveRDS(lte_001, "output/2019-07-19/ct-scores/ct-scores_lte001_2019-07-19.rds")
rm(lte_001)

saveRDS(gt_001, "output/2019-07-19/ct-scores/ct-scores_gt001_2019-07-19.rds")


