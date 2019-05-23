library(keras)
library(raster)
library(coolit)
library(parallel)
library(sf)
library(data.table)

scored <- fs::dir_ls("output/2019-05-07/scored_chi", type = "file")

scored <- stringr::str_match(scored, "(.*/)(\\d{1,4}).*")[, 3]

sliced <- fs::dir_ls("data/source_from-chi-website/chi-slices", type = "file")

sliced <- data.frame(
  sliced,
  sliced_img_nums = as.numeric(
    stringr::str_match(sliced, "(.*/)(\\d{1,4}).*")[, 3]
    ),
  stringsAsFactors = FALSE
)

to_score <- sliced[!(as.character(sliced$sliced_img_nums) %in% scored),]

fs::file_copy(to_score$sliced, "D:/wfu3/coolit/output/2019-05-07/chi-slices")

# score slices
scores <- score_slice_data_dir(
  slice_data_dir = "D:/wfu3/coolit/output/2019-05-07/chi-slices",
  model_params_dput_file = "output/multi-model-runs/2019-05-07/models/2019-05-07_17-15-20/run-parameters_dput.txt",
  model_h5_weights = "output/multi-model-runs/2019-05-07/models/2019-05-07_17-15-20/model_fine-tune-1.h5",
  score_outdir = "D:/wfu3/coolit/output/2019-05-07/chi-scores",
  return_score = FALSE
)

# collate scores
ncores <- detectCores() - 1

my_scores_files <- list.files("output/2019-05-07/chi-scores",
                              full.names = TRUE)

my_scores_id <- make_split_index(length(my_scores_files), ncores)

my_scores_list <- split(my_scores_files, my_scores_id)

cl <- parallel::makeCluster(ncores)
parallel::clusterEvalQ(cl, {
  library(data.table)
})

my_scores <- parLapply(
  my_scores_list,
  cl = cl,
  fun = function(x) {
    temp <- lapply(x, function(y) {
      temp <- readRDS(y)
      temp <- data.table(temp[["slice_data"]])
      temp$img_name <- y

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

saveRDS(my_scores, "output/2019-05-07/chi-scores/chi_all-scores.rds")

# save scores <= 0.001
my_scores_lte001 <- rbindlist(lapply(my_scores, function(x) x[["lte001"]]))

saveRDS(my_scores_lte001, "output/2019-05-07/chi-scores/chi-scores-lte001.rds")
rm(my_scores_lte001)
gc()

# save scores > 0.001 in total and in 10 equal-sized chunks
my_scores_gt001 <- rbindlist(lapply(my_scores, function(x) x[["gt001"]]))
rm(my_scores)
gc()

my_scores_gt001$geometry <- st_sfc(my_scores_gt001$geometry)
my_scores_gt001 <- st_sf(my_scores_gt001)

saveRDS(my_scores_gt001, "output/2019-05-07/chi-scores/chi-scores-gt001.rds")

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
          paste0("output/2019-05-07/chi-scores/chi-scores-",
                 "gt", out_stub_low, "-",
                 "lte", out_stub_high, ".rds"),
          compress = FALSE)
}

