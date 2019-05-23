#' Train tower model with keras
#'
#' @export
#' @importFrom keras
#' image_data_generator flow_images_from_directory
#' keras_model_sequential layer_flatten layer_dense layer_dropout
#' freeze_weights compile callback_model_checkpoint callback_csv_logger
#' fit_generator unfreeze_weights callback_reduce_lr_on_plateau
#' application_densenet  application_densenet201  application_xception
#' application_nasnet  application_mobilenet_v2  application_inception_resnet_v2
#' application_inception_v3  application_resnet50  application_vgg16
#' application_vgg19  application_densenet121  application_nasnetmobile
#' application_nasnetlarge  application_densenet169  application_mobilenet
#' optimizer_adagrad  optimizer_rmsprop  optimizer_nadam  optimizer_adadelta
#' optimizer_adam  optimizer_adamax optimizer_sgd
#'
#' @importFrom stringr str_detect str_replace
train_tower_model <- function(
  # directories
  img_base_dir = "data/tiles/splits",
  img_dir = "orig",
  output_dir = "output",

  # images
  img_size = c(50, 50),

  # training image params
  img_horizontal_flip = TRUE,
  img_vertical_flip = TRUE,
  batch_size = 32,

  # training model params
  base_model = "vgg16",
  save_best_model_only = TRUE,

  # dense model structure
  add_small_final_layer = FALSE,
  small_layer_size = NULL,

  # dense model params
  dense_structure = list(
    list(units = 256, dropout = 0.2),
    list(units = 128, dropout = 0.2)
  ),
  dense_optimizer = "rmsprop",
  dense_lr = 1e-5,
  dense_steps_per_epoch = 100,
  dense_epochs = 30,
  dense_validation_steps = 50,

  # first fine-tune model params
  first_ft_unfreeze = "block4_conv1",
  first_ft_optimizer = "rmsprop",
  first_ft_lr = 1e-5,
  first_ft_steps_per_epoch = 100,
  first_ft_epochs = 30,
  first_ft_validation_steps = 50,

  # second fine-tune model params
  do_second_ft = TRUE,
  second_ft_unfreeze = "block3_conv1",
  second_ft_optimizer = "rmsprop",
  second_ft_lr = 5e-6,
  second_ft_steps_per_epoch = 100,
  second_ft_epochs = 30,
  second_ft_validation_steps = 50,

  # class weights
  class_weights = NULL

) {

  # suppressMessages({
  #   library(keras)
  #   library(pbapply)
  #   library(ggplot2)
  #   library(stringr)
  #   library(ROCR)
  #   library(abind)
  # })

  message("Here we go!")

  params <- as.list(environment())

  #### error checks --------------------------------
  # required arguments exists?
  args_null <- stats::setNames(sapply(params, is.null), names(params))
  req_args_null <- args_null[!(names(args_null) == "small_layer_size") &
                               !(grepl("second_ft_", names(args_null)))]

  if (any(req_args_null)) {
    stop("All function arguments except `small_layer_size` ",
         "and `second_ft_*` are required")
  }

  if (params$do_second_ft) {
    second_ft_args_null <- args_null[grepl("second_ft_", names(args_null))]

    if (any(second_ft_args_null)) {
      stop("If `do_second_ft` is TRUE, all",
           "`second_ft_*` arguments are required")
    }
  }

  if (params$add_small_final_layer && is.null(params$small_layer_size)) {
    stop("If `add_small_final_layer` is TRUE, ",
         "`small_layer_size` argument is required.")
  }

  # error check: source directories
  if (!dir.exists(img_base_dir)) {
    stop("`img_base_dir` does not exist!")
  }

  if (!dir.exists(file.path(img_base_dir, img_dir))) {
    stop("`img_dir` does not exist!")
  }

  if (!dir.exists(output_dir)) {
    stop("`output_dir` does not exist!")
  }

  # error check: image params
  if (!(is.numeric(img_size) && length(img_size) == 2)) {
    stop("`img_size` must be a numeric vector of length 2")
  }

  if (!(is.numeric(batch_size) && length(batch_size) == 1)) {
    stop("Image flow parameter `batch_size` must be a numeric vector of length 1")
  }

  # error check: base model
  keras_funs <- getNamespaceExports("keras")
  base_model_options <- keras_funs[str_detect(keras_funs, "application_")]
  base_model_options <- str_replace(base_model_options, "application_", "")

  if (!(params$base_model %in% base_model_options)) {
    stop("Base model choice not available in keras package!")
  }

  # error check: optimizers
  optimizer_options <- keras_funs[str_detect(keras_funs, "optimizer_")]
  optimizer_options <- str_replace(optimizer_options, "optimizer_", "")

  if (!(params$dense_optimizer %in% optimizer_options)) {
    stop("Dense optimizer choice not available in keras package!")
  }

  if (!(params$first_ft_optimizer %in% optimizer_options)) {
    stop("First fine-tune optimizer choice not available in keras package!")
  }

  if (!(params$second_ft_optimizer %in% optimizer_options)) {
    stop("Second fine-tune optimizer choice not available in keras package!")
  }

  # error check: model params
  model_param_args <- params[grepl("_lr", names(params)) |
                               grepl("_steps_per_epoch", names(params)) |
                               grepl("_epochs", names(params)) |
                               grepl("_validation_steps", names(params))]
  model_param_args <- sapply(model_param_args, function(x) {
    is.numeric(x) & length(x) == 1
  })

  if (any(!model_param_args)) {
    stop("All arguments *_lr, *_steps_per_epoch, *_epochs, *_validation_steps ",
         "must be single numeric values")
  }

  if (!is.null(class_weights)) {
    if (!is.list(class_weights) || length(class_weights) != 2 ||
        !is.numeric(class_weights[[1]]) || !is.numeric(class_weights[[2]]) ||
        length(names(class_weights)) != 2 || ("" %in% names(class_weights))) {
      stop("Argument `class_weights` must be a named list of length 2 ",
           "containing numeric values.")
    }
  }

  # create and error check directories
  params$train_dir <- file.path(params$img_base_dir, params$img_dir, "train")
  params$valid_dir <- file.path(params$img_base_dir, params$img_dir, "validation")

  if (!(dir.exists(params$train_dir) && dir.exists(params$valid_dir))) {
    stop("`img_dir` must contain 2 directories named 'train' and 'validation'")
  }

  if (!(dir.exists(file.path(params$train_dir, "tower")) &&
        dir.exists(file.path(params$train_dir, "notower")) &&
        dir.exists(file.path(params$valid_dir, "tower")) &&
        dir.exists(file.path(params$valid_dir, "notower")))) {
    stop("Both 'train' and 'validation' directories must contain 2 ",
         "directories names 'tower' and 'notower'")
  }

  params$num_train_tower <- length(list.files(file.path(params$train_dir, "tower")))
  params$num_train_notower <- length(list.files(file.path(params$train_dir, "notower")))

  params$output_dir <- file.path(params$output_dir, Sys.Date())
  dir.create(params$output_dir)

  params$models_dir <- file.path(params$output_dir, "models")
  dir.create(params$models_dir)

  # make class weights
  if (is.null(class_weights)) {
    params$class_weights <- list(
      `1` = (params$num_train_tower + params$num_train_notower) / params$num_train_tower,
      `0` = (params$num_train_tower + params$num_train_notower) / params$num_train_notower
    )

    message("\nTower distribution in training data: \n",
            "   Prop. tower = ", round(1 / params$class_weights$`1`, 3), "\n",
            "   Prop. no tower = ", round(1 / params$class_weights$`0`, 3)
    )
  } else {
    params$class_weights <- class_weights
  }

  #### initiate model objects ----------------------------
  train_datagen <- image_data_generator(
    rescale = 1/255,
    horizontal_flip = params$img_horizontal_flip,
    vertical_flip = params$img_vertical_flip)

  train_flow <- flow_images_from_directory(
    params$train_dir,
    train_datagen,
    target_size = params$img_size,
    batch_size = params$batch_size,
    class_mode = "binary"
  )

  valid_datagen <- image_data_generator(rescale = 1/255)

  valid_flow <- flow_images_from_directory(
    params$valid_dir,
    valid_datagen,
    target_size = params$img_size,
    batch_size = params$batch_size,
    class_mode = "binary"
  )

  #### freeze base net ---------------------------------
  conv_base <- do.call(paste0("application_", params$base_model),
                       args = list(weights = "imagenet",
                                   include_top = FALSE,
                                   input_shape = c(params$img_size, 3)))

  #### train dense layers ---------------------------------
  model <- keras_model_sequential() %>%
    conv_base %>%
    layer_flatten()

  for (i in seq_along(params$dense_structure)) {
    model <- model %>%
      layer_dense(units = params$dense_structure[[i]][["units"]],
                  activation = "relu")

    if (params$dense_structure[[i]][["dropout"]] > 0) {
      model <- model %>%
        layer_dropout(rate = params$dense_structure[[i]][["dropout"]])
    }
  }

  if (params$add_small_final_layer) {
    model <- model %>%
      layer_dense(units = params$small_layer_size, activation = "relu")
  }

  model <- model %>%
    layer_dense(units = 1, activation = "sigmoid")

  freeze_weights(conv_base)

  params$curr_model_dir <- file.path(params$models_dir, format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
  dir.create(params$curr_model_dir)

  sink(file = file.path(params$curr_model_dir, "model-structure.txt"))
  print(summary(model))
  sink()

  params$dense_trainable_weights <- length(model$trainable_weights)

  my_optimizer <- do.call(paste0("optimizer_", params$dense_optimizer),
                          args = list(lr = params$dense_lr))

  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = my_optimizer,
    metrics = c("accuracy")
  )

  callback_list <- list(
    callback_model_checkpoint(
      filepath = file.path(params$curr_model_dir, "model_train-dense.h5"),
      monitor = "val_loss",
      save_best_only = params$save_best_model_only
    ),
    callback_csv_logger(
      filename = file.path(params$curr_model_dir, "log_train-dense.csv")
    )
  )

  message("\nTraining dense model:")
  before <- Sys.time()

  history <- model %>% fit_generator(
    train_flow,
    steps_per_epoch = params$dense_steps_per_epoch,
    epochs = params$dense_epochs,
    validation_data = valid_flow,
    validation_steps = params$dense_validation_steps,
    callbacks = callback_list,
    class_weight = params$class_weights
  )

  params$dense_training_time <- Sys.time() - before
  message("Dense model took ",
          round(params$dense_training_time, 3),
          " ",
          attr(params$dense_training_time, "units"),
          " to train.")

  params$dense_training_history <- history
  message("Best dense model validation metrics: \n",
          "   Loss = ", round(min(history$metrics$val_loss, na.rm = TRUE), 3), "\n",
          "   Accuracy = ",
          round(history$metrics$val_acc[
            which(history$metrics$val_loss == min(history$metrics$val_loss, na.rm = TRUE))],
            3))

  #### train first fine-tune model -------------------------------
  length(model$trainable_weights)

  unfreeze_weights(conv_base, from = params$first_ft_unfreeze)

  params$first_ft_trainable_weights <- length(model$trainable_weights)

  my_optimizer <- do.call(paste0("optimizer_", params$first_ft_optimizer),
                          args = list(lr = params$first_ft_lr))

  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = my_optimizer,
    metrics = c("accuracy")
  )

  callback_list <- list(
    callback_model_checkpoint(
      filepath = file.path(params$curr_model_dir, "model_fine-tune-1.h5"),
      monitor = "val_loss",
      save_best_only = params$save_best_model_only
    ),
    callback_csv_logger(
      filename = file.path(params$curr_model_dir, "log_fine-tune-1.csv")
    ),
    callback_reduce_lr_on_plateau()
  )

  message("\nTraining first fine-tune model:")
  before <- Sys.time()

  history <- model %>% fit_generator(
    train_flow,
    steps_per_epoch = params$first_ft_steps_per_epoch,
    epochs = params$first_ft_epochs,
    validation_data = valid_flow,
    validation_steps = params$first_ft_validation_steps,
    callbacks = callback_list
  )

  params$first_ft_training_time <- Sys.time() - before
  message("First first fine-tune model took ",
          round(params$first_ft_training_time, 3),
          " ",
          attr(params$first_ft_training_time, "units"),
          " to train.")

  params$first_ft_training_history <- history
  message("Best first fine-tune model validation metrics: \n",
          "   Loss = ", round(min(history$metrics$val_loss, na.rm = TRUE), 3), "\n",
          "   Accuracy = ",
          round(history$metrics$val_acc[
            which(history$metrics$val_loss == min(history$metrics$val_loss, na.rm = TRUE))],
            3))

  #### train second fine-tune model -------------------------------
  if (params$do_second_ft) {
    unfreeze_weights(conv_base, from = params$second_ft_unfreeze)

    params$second_ft_trainable_weights <- length(model$trainable_weights)

    my_optimizer <- do.call(paste0("optimizer_", params$second_ft_optimizer),
                            args = list(lr = params$second_ft_lr))

    model %>% compile(
      loss = "binary_crossentropy",
      optimizer = my_optimizer,
      metrics = c("accuracy")
    )

    callback_list <- list(
      callback_model_checkpoint(
        filepath = file.path(params$curr_model_dir, "model_fine-tune-2.h5"),
        monitor = "val_loss",
        save_best_only = params$save_best_model_only
      ),
      callback_csv_logger(
        filename = file.path(params$curr_model_dir, "log_fine-tune-2.csv")
      ),
      callback_reduce_lr_on_plateau()
    )

    message("\nTraining second fine-tune model:")
    before <- Sys.time()

    history <- model %>% fit_generator(
      train_flow,
      steps_per_epoch = params$second_ft_steps_per_epoch,
      epochs = params$second_ft_epochs ,
      validation_data = valid_flow,
      validation_steps = params$second_ft_validation_steps,
      callbacks = callback_list
    )

    params$second_ft_training_time <- Sys.time() - before
    message("Second fine-tune model took ",
            round(params$second_ft_training_time, 3),
            " ",
            attr(params$second_ft_training_time, "units"),
            " to train.")

    params$second_ft_training_history <- history
    message("Best second fine-tune model validation metrics: \n",
            "   Loss = ", round(min(history$metrics$val_loss, na.rm = TRUE), 3), "\n",
            "   Accuracy = ",
            round(history$metrics$val_acc[
              which(history$metrics$val_loss == min(history$metrics$val_loss, na.rm = TRUE))],
              3))
  }

  sink(file = file.path(params$curr_model_dir, "run-parameters.txt"))
  print(params)
  sink()
  sink(file = file.path(params$curr_model_dir, "run-parameters_dput.txt"))
  dput(params)
  sink()
}
