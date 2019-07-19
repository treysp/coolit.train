library(keras)
library(raster)
library(coolit.train)
library(sf)

# score slices
scores <- score_slice_data_dir(
  slice_data_dir = "~/coolit.train/data/curated-training-slices/ct/ct-slices",
  model_params_dput_file = "~/coolit.train/output/multi-model-runs/2019-07-02/models/2019-07-02_11-08-43/run-parameters_dput.txt",
  model_h5_weights = "~/coolit.train/output/multi-model-runs/2019-07-02/models/2019-07-02_11-08-43/model_fine-tune-2.h5",
  score_outdir = "~/coolit.train//coolit.train/output/2019-07-19/ct-scores",
  return_score = FALSE
)