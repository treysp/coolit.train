library(coolit)
library(tensorflow)
library(keras)
library(pbapply)
library(ggplot2)
library(stringr)
library(ROCR)
library(abind)

#### meta-parameters -----------------------
meta_params <- list(
  img_dir = c("slices_curated_2019-05-16"),

  base_model = c("vgg16"),

  small_final_layer = list(
    list(NA)
    #list(small_layer_size = 4, dense_epochs = 60),
    #list(small_layer_size = 8, dense_epochs = 60)
  ),

  class_weights = list(
    list(`1` = 4, `0` = 1),
    list(`1` = 6, `0` = 1),
    list(`1` = 8, `0` = 1)
  )
)

meta_params <- expand.grid(meta_params,
                           KEEP.OUT.ATTRS = FALSE,
                           stringsAsFactors = FALSE)
meta_params <- split(meta_params, seq_len(NROW(meta_params)))

#### parameters ---------------------------
params <- list(

  # directories
  img_base_dir = "data/model-training-data",
  img_dir = NULL,
  output_dir = "output/multi-model-runs",

  # images
  img_size = c(50, 50),

  # training image params
  img_horizontal_flip = TRUE,
  img_vertical_flip = TRUE,
  batch_size = 32,

  # training model params
  base_model = NULL,
  save_best_model_only = TRUE,

  # dense model structure
  add_small_final_layer = NULL,
  small_layer_size = NA,

  # dense model params
  dense_structure = list(
    list(units = 256, dropout = 0.2),
    list(units = 128, dropout = 0.2)
  ),
  dense_optimizer = "rmsprop",
  dense_lr = 1e-5,
  dense_steps_per_epoch = 100,
  dense_epochs = 50,
  dense_validation_steps = 50,

  # first fine-tune model params
  first_ft_unfreeze = "block4_conv1",
  first_ft_optimizer = "rmsprop",
  first_ft_lr = 1e-5,
  first_ft_steps_per_epoch = 100,
  first_ft_epochs = 50,
  first_ft_validation_steps = 50,

  # second fine-tune model params
  do_second_ft = FALSE,
  second_ft_unfreeze = "block3_conv1",
  second_ft_optimizer = "rmsprop",
  second_ft_lr = 5e-6,
  second_ft_steps_per_epoch = 100,
  second_ft_epochs = 50,
  second_ft_validation_steps = 50,

  # class weights
  class_weights = NULL

)

#### execution ----------------------------
for (i in seq_along(meta_params)) {
  curr_params <- meta_params[[i]]

  params$img_dir <- curr_params[["img_dir"]]
  params$base_model <- curr_params[["base_model"]]
  params$class_weights <- curr_params[["class_weights"]]


  if (!is.na(curr_params[["small_final_layer"]][[1]][[1]])) {
    params$add_small_final_layer <- TRUE
    params$small_layer_size <- curr_params[["small_final_layer"]][[1]][["small_layer_size"]]
    params$dense_epochs <- curr_params[["small_final_layer"]][[1]][["dense_epochs"]]
  } else {
    params$add_small_final_layer <- FALSE
  }

  message("Current parameter set: \n",
          "   Image = '", curr_params[["img_dir"]], "'\n",
          "   Base model = '", curr_params[["base_model"]], "'\n",
          "   Small final layer = ", curr_params[["small_final_layer"]], "\n",
          "   Class weights = ",
            paste0(curr_params[["class_weights"]][[1]][[1]], ":",
                   curr_params[["class_weights"]][[1]][[2]], "\n"))

  do.call("train_tower_model", params)
}
