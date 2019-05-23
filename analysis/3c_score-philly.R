library(keras)
library(raster)
library(coolit)
library(parallel)
library(sf)
library(data.table)

# score slices
scores <- score_slice_data_dir(
  slice_data_dir = "F:/wfu3/coolit/output/2019-04-18/sliced_philly",
  model_params_dput_file = "output/multi-model-runs/2019-03-04/models/2019-03-04_20-49-33/run-parameters_dput.txt",
  model_h5_weights = "output/multi-model-runs/2019-03-04/models/2019-03-04_20-49-33/model_fine-tune-2.h5",
  score_outdir = "F:/wfu3/coolit/output/2019-04-18/scored_philly",
  return_score = FALSE
)

# collate scores
my_scores_files <- list.files("f:/wfu3/coolit/output/2019-04-18/scored_philly/",
                              full.names = TRUE)

my_scores_id <- rep(seq(1, detectCores() - 1),
                    each = length(my_scores_files) %/% (detectCores() - 1))
my_scores_id <- c(my_scores_id,
                  rep(max(my_scores_id),
                      length(my_scores_files) - length(my_scores_id)))

my_scores_list <- split(my_scores_files, my_scores_id)

cl <- parallel::makeCluster(detectCores() - 1)
parallel::clusterEvalQ(cl, {
  library(data.table)
  library(sf)
})

my_scores <- pbapply::pblapply(
  my_scores_list,
  cl = cl,
  FUN = function(x) {
    temp <- lapply(x, function(y) {
      temp <- readRDS(y)
      temp <- data.table(temp[["slice_data"]])
      temp$geometry <- do.call("c", temp$geometry)

      list(
        lte001 = temp[predicted_probs <= 0.001],
        gt001 = temp[predicted_probs > 0.001]
      )
    })

    list(
      lte001 = rbindlist(lapply(temp, function(z) z[["lte001"]])),
      gt001 = rbindlist(lapply(temp, function(z) z[["gt001"]]))
    )
  })

stopCluster(cl)

# save scores <= 0.001
my_scores_lte001 <- rbindlist(lapply(my_scores, function(x) x[["lte001"]]))
my_scores_lte001$geometry <- st_sfc(my_scores_lte001$geometry)
my_scores_lte001 <- st_sf(my_scores_lte001)

saveRDS(my_scores_lte001, "output/2019-04-18/scored_philly/philly-scores-lte001.rds")
rm(my_scores_lte001)
gc()

# save scores > 0.001 in total and in 10 equal-sized chunks
my_scores_gt001 <- rbindlist(lapply(my_scores, function(x) x[["gt001"]]))
my_scores_gt001$geometry <- st_sfc(my_scores_gt001$geometry)
my_scores_gt001 <- st_sf(my_scores_gt001)

saveRDS(my_scores_gt001, "output/2019-04-18/scored_philly/philly-scores-gt001.rds")

my_scores_gt001$predicted_probs_rd4 <- round(my_scores_gt001$predicted_probs, 4)
my_quant <- quantile(my_scores_gt001$predicted_probs_rd4, c(seq(0.1, 1, by = 0.1)))

for (i in seq_len(length(my_quant) - 1)) {
  temp <- my_scores_gt001[
    my_scores_gt001$predicted_probs_rd4 > my_quant[i] &
      my_scores_gt001$predicted_probs_rd4 <= my_quant[i + 1],
  ]

  if (i == 1) {
    out_stub_low <- "001"
  } else {
    out_stub_low <- out_stub_low <- as.character(my_quant[i])
    out_stub_low <- gsub("0\\.", "", out_stub_low)
  }

  if (i == (length(my_quant) - 1)) {
    out_stub_high <- "1"
  } else {
    out_stub_high <- as.character(my_quant[i + 1])
    out_stub_high <- gsub("0\\.", "", out_stub_high)
  }

  saveRDS(temp,
          paste0("output/2019-04-18/scored_philly/philly-scores-",
                 "gt", out_stub_low, "-",
                 "lte", out_stub_high, ".rds"),
          compress = FALSE)
}

