library(ggplot2)
library(stringr)
library(dplyr)

model_run_date <- "2019-07-02"

confusion_files <- list.files(paste0("output/multi-model-runs/",
                                     model_run_date,
                                     "/models"),
                              recursive = TRUE,
                              full.names = TRUE,
                              pattern = "confusion")

confusion_dirs <- str_match(confusion_files, "(.*/)*(.*)")[, 2]

confusion <- lapply(confusion_dirs, function(x) {
  params <- eval(parse(
    paste0(x, "run-parameters_dput.txt")
    )
  )

  confusion <- read.csv(paste0(x, "valid-confusion-matrix.csv"),
                        stringsAsFactors = FALSE)

  list(params = params, confusion = confusion)
})

# what meaningful aspects of models were different?
confusion_compares <- combn(length(confusion), 2, simplify = FALSE)

confusion_compares <- lapply(confusion_compares, function(x) {
  out <- all.equal(confusion[[x[1]]]$params,
                   confusion[[x[2]]]$params)

  out <- str_replace(out, '“', '"')
  out <- str_replace(out, '”', '"')

  out <- str_split(out, '"')

  out <- sapply(out, function(x) x[2])

  out <- out[!(str_detect(out, "time") |
                 str_detect(out, "history") |
                 str_detect(out, "model_dir") |
                 str_detect(out, "trainable_weights"))]

  out
})

confusion_compares <- unique(unlist(confusion_compares))

confusion_compares <- lapply(confusion, function(x) {
  x$params[confusion_compares]
})

names(confusion_compares) <- str_match(confusion_dirs, "(.*/)*(.*)/$")[, 3]

# make curves
plot_data <- lapply(seq_along(confusion), function(i){
  out <- confusion[[i]]$confusion

  model_time <- str_replace(confusion_dirs[i], "/$", "")
  model_time <- str_match(model_time, "(.*/)*(.*)")[, 3]

  out$model_time <- model_time
  out
})

plot_data <- do.call("rbind", plot_data)

ggplot(plot_data) +
  geom_line(aes(x = sens_recall, y = ppv_precision, color = model_time)) +
  scale_color_viridis_d() +
  theme_minimal()

max_sens <- plot_data %>%
  mutate(
    ppv_precision2 = round(ppv_precision, 2)
  ) %>%
  group_by(ppv_precision2) %>%
  mutate(
    max_sens_recall = max(sens_recall)
    ) %>%
  ungroup() %>%
  filter(sens_recall == max_sens_recall &
           sens_recall >= 0.75 &
           ppv_precision >= 0.75) %>%
  group_by(model_time) %>%
  tally() %>%
  arrange(-n)

best_models <- sort(max_sens[["model_time"]])

ggplot(plot_data %>% filter(model_time %in% best_models)) +
  geom_line(aes(x = sens_recall, y = ppv_precision, color = model_time)) +
  scale_color_viridis_d() +
  xlab("Sensitivity/Recall") +
  ylab("PPV/Precision") +
  ggtitle("Sensitivity vs. PPV by model") +
  theme_minimal()

# inspect best models
mae <- lapply(best_models, function(model) {

  pred_prob <- read.csv(
    paste0("output/multi-model-runs/",
           model_run_date,
           "/models/",
           model,
           "/valid-img-scores.csv"),
    stringsAsFactors = FALSE
  )

  pred_prob$model_time <- model

  list(
    pred_prob = pred_prob,
    mae = list(
      tower = abs(mean(pred_prob$pred_prob[pred_prob$truth == 1]) - 1),
      notower = abs(mean(pred_prob$pred_prob[pred_prob$truth == 0]))
      )
  )
})

names(mae) <- best_models

mae <- purrr::transpose(mae)

mae$pred_prob <- do.call("rbind", mae$pred_prob)

ggplot(mae$pred_prob) +
  geom_smooth(aes(x = pred_prob, y = truth, color = model_time),
              se = FALSE) +
  geom_abline(intercept = 0, slope =1 ) +
  scale_color_viridis_d() +
  theme_minimal()
