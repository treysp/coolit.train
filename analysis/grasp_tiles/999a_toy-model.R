library(keras)
library(pbapply)
library(ggplot2)
library(ROCR)

train_dir <- "data/tiles/splits/rgb_lin-stretch/train"
valid_dir <- "data/tiles/splits/rgb_lin-stretch/test"
output_dir <- "output"

num_train_tower <- length(list.files(file.path(train_dir, "tower")))
num_train_notower <- length(list.files(file.path(train_dir, "notower")))

class_weights <- list(
  `1` = (num_train_tower + num_train_notower) / num_train_tower,
  `0` = (num_train_tower + num_train_notower) / num_train_notower
)

(c(`Prop. tower` = 1 / class_weights$`1`, `Prop. No tower` = 1 / class_weights$`0`))

output_dir <- file.path(output_dir, Sys.Date())
dir.create(output_dir)

models_dir <- file.path(output_dir, "models")
dir.create(models_dir)

# create data objects
train_datagen <- image_data_generator(
  rescale = 1/255,
  horizontal_flip = TRUE,
  vertical_flip = TRUE)

train_flow <- flow_images_from_directory(
  train_dir, 
  train_datagen,
  target_size = c(50, 50),
  batch_size = 32,
  class_mode = "binary"
)

valid_datagen <- image_data_generator(rescale = 1/255)

valid_flow <- flow_images_from_directory(
  valid_dir, 
  valid_datagen,
  target_size = c(50, 50),
  batch_size = 32,
  class_mode = "binary"
)


# freeze base net and train dense layers on top
conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(50, 50, 3)
)

model <- keras_model_sequential() %>% 
  conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 1, activation = "sigmoid")

length(model$trainable_weights)

freeze_weights(conv_base)

length(model$trainable_weights)
summary(model)

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-5),
  metrics = c("accuracy")
)

curr_model_dir <- file.path(models_dir, format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
dir.create(curr_model_dir)

callback_list <- list(
  callback_model_checkpoint(
    filepath = file.path(curr_model_dir, "model_train-dense.h5"),
    monitor = "val_loss",
    save_best_only = TRUE
  ),
  callback_csv_logger(
    filename = file.path(curr_model_dir, "log_train-dense.csv")
  )
)

history <- model %>% fit_generator(
  train_flow,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = valid_flow,
  validation_steps = 50,
  callbacks = callback_list,
  class_weight = class_weights
)

length(model$trainable_weights)

unfreeze_weights(conv_base, from = "block4_conv1")

length(model$trainable_weights)

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-5),
  metrics = c("accuracy")
)

callback_list <- list(
  callback_model_checkpoint(
    filepath = file.path(curr_model_dir, "model_fine-tune-1.h5"),
    monitor = "val_loss",
    save_best_only = TRUE
  ),
  callback_csv_logger(
    filename = file.path(curr_model_dir, "log_fine-tune-1.csv")
  ),
  callback_reduce_lr_on_plateau()
)

history <- model %>% fit_generator(
  train_flow,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = valid_flow,
  validation_steps = 50,
  callbacks = callback_list
)

length(model$trainable_weights)

unfreeze_weights(conv_base, from = "block3_conv1")

length(model$trainable_weights)

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 5e-6),
  metrics = c("accuracy")
)

callback_list <- list(
  callback_model_checkpoint(
    filepath = file.path(curr_model_dir, "model_fine-tune-2.h5"),
    monitor = "val_loss",
    save_best_only = TRUE
  ),
  callback_csv_logger(
    filename = file.path(curr_model_dir, "log_fine-tune-2.csv")
  ),
  callback_reduce_lr_on_plateau()
)

history <- model %>% fit_generator(
  train_flow,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = valid_flow,
  validation_steps = 50,
  callbacks = callback_list
)

# examine predictions
valid_files <- list.files(valid_dir, full.names = TRUE, recursive = TRUE)

img_dims <- dim(image_to_array(image_load(valid_files[1])))

img_to_score <- pblapply(valid_files, function(img) {
 out <- image_load(img)
 out <- image_to_array(out)
 out <- array_reshape(out, c(1, img_dims))
 out <- out / 255
 out
})

img_to_score <- abind::abind(img_to_score, along = 1)

predicted_probs <- data.frame(pred_prob = predict_proba(model, img_to_score))
predicted_probs[["img_name"]] <- valid_files
predicted_probs[["truth"]] <- as.numeric(!stringr::str_detect(valid_files, "notower"))

write.csv(predicted_probs, 
          file = file.path(curr_model_dir, "predicted-probs.csv"),
          row.names = FALSE)

ggplot(predicted_probs) +
  geom_histogram(aes(x = pred_prob, fill = factor(truth)), 
                 binwidth = .01, color = NA, alpha = .4) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()

pred <- prediction(predicted_probs$pred_prob, predicted_probs$truth)
performance(pred, "auc")@y.values
plot(performance(pred, "acc"))
plot(performance(pred, "tpr", "spec"))
plot(performance(pred, "npv", "ppv"))


