library(coolit.train)
library(tensorflow)
library(keras)
library(ggplot2)
library(stringr)

#### meta-parameters -----------------------
meta_params <- list(
  base_model = c("vgg16"),

  batch_size = c(192),

  optimizer = c("adam"),

  dense_lr = c(1e-5),

  dense_structure = list(
    list(
      list(units = 512, dropout = 0.3),
      list(units = 128, dropout = 0.3)
    )
    ),

  class_weights = list(
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
  img_dir = "data/model-training-data/slices_curated_2019-05-16",
  output_dir = "output/multi-model-runs",

  # images
  img_size = c(50, 50),

  # training image params
  img_horizontal_flip = TRUE,
  img_vertical_flip = TRUE,
  batch_size = NULL,

  # training model params
  base_model = NULL,
  save_best_model_only = TRUE,

  # dense model params
  dense_structure = NULL,
  dense_optimizer = NULL,
  dense_lr = NULL,
  dense_steps_per_epoch = 100,
  dense_epochs = 75,
  dense_validation_steps = 50,

  # first fine-tune model params
  first_ft_unfreeze = "block4_conv1",
  first_ft_optimizer = NULL,
  first_ft_lr = 1e-5,
  first_ft_steps_per_epoch = 100,
  first_ft_epochs = 75,
  first_ft_validation_steps = 50,

  # second fine-tune model params
  do_second_ft = TRUE,
  second_ft_unfreeze = "block3_conv1",
  second_ft_optimizer = "adam",
  second_ft_lr = 5e-6,
  second_ft_steps_per_epoch = 100,
  second_ft_epochs = 75,
  second_ft_validation_steps = 50,

  # class weights
  class_weights = NULL

)

#### execution ----------------------------
for (i in seq_along(meta_params)) {
  curr_params <- meta_params[[i]]

  params$batch_size <- curr_params[["batch_size"]]
  params$base_model <- curr_params[["base_model"]]
  params$class_weights <- curr_params[["class_weights"]][[1]]

  params$dense_structure <- curr_params[["dense_structure"]][[1]]
  params$dense_optimizer <- curr_params[["optimizer"]]
  params$dense_lr <- curr_params[["dense_lr"]]

  params$first_ft_optimizer <- curr_params[["optimizer"]]

  message("Current parameter set: \n",
          "   Base model = '", curr_params[["base_model"]], "'\n",
          "   Batch size = '", curr_params[["batch_size"]], "'\n",
          "   Class weights = ",
            paste0(curr_params[["class_weights"]][[1]][[1]], ":",
                   curr_params[["class_weights"]][[1]][[2]], "\n"),
          "   Optimizer = '", curr_params[["optimizer"]], "'\n",
          "   Dense LR = '", curr_params[["dense_lr"]], "'\n")

  do.call("train_tower_model", params)
}
