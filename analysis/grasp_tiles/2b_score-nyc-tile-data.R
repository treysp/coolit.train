library(keras)
library(raster)
library(coolit)

scores <- score_tile_data_dir(
  tile_data_dir = "F:/coolit/output/2019-03-31/sliced_nyc",
  model_params_dput_file = "output/multi-model-runs/2019-03-04/models/2019-03-04_20-49-33/run-parameters_dput.txt",
  model_h5_weights = "output/multi-model-runs/2019-03-04/models/2019-03-04_20-49-33/model_fine-tune-2.h5",
  score_outdir = "F:/coolit/output/2019-03-31/scored_nyc_pre-sliced/",
  return_score = FALSE
)
