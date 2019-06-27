library(keras)
library(raster)
library(coolit.train)
library(parallel)
library(sf)
library(data.table)
library(fs)

slice_dir <- "data/curated-training-slices/nj-nearmap/nj-nearmap-slices"

out_dir <- "D:/wfu3/coolit.train/output/2019-05-28/nj-nearmap-scores/"

all_slices <- fs::dir_ls(slice_dir, type = "file")

all_slices_info <- fs::file_info(all_slices)
all_slices_size <- fs::fs_bytes(sum(all_slices_info$size))
num_chunks <- ceiling(all_slices_size / "333G")

all_slices_chunks <- split(all_slices, ifelse(
  (seq_along(all_slices) / length(all_slices)) <= .33,
  1,
  ifelse((seq_along(all_slices) / length(all_slices)) > .33 &
           (seq_along(all_slices) / length(all_slices)) <= .66,
  2,
  3)
  )
)

for (i in seq_along(all_slices_chunks)) {
  cat(i, "\n")
  file_delete(
    dir_ls("D:/wfu3/coolit.train/source_from-nj-nearmap-website/nj-nearmap-slices",
           type = "file")
    )

  file_copy(all_slices_chunks[[i]],
            "D:/wfu3/coolit.train/source_from-nj-nearmap-website/nj-nearmap-slices"
            )

  scores <- score_slice_data_dir(
    slice_data_dir = "D:/wfu3/coolit.train/source_from-nj-nearmap-website/nj-nearmap-slices",
    model_params_dput_file = file.path("output/multi-model-runs/2019-05-28/models",
                                       "2019-05-28_16-03-07/run-parameters_dput.txt"),
    model_h5_weights = file.path("output/multi-model-runs/2019-05-28/models",
                                 "2019-05-28_16-03-07/model_fine-tune-2.h5"),
    score_outdir = out_dir,
    return_score = FALSE
  )

  # collate scores
  my_scores_files <- list.files(out_dir, full.names = TRUE)

  my_scores_id <- make_split_index(length(my_scores_files), 20)

  my_scores_list <- split(my_scores_files, my_scores_id)

  cl <- parallel::makeCluster(20)
  parallel::clusterEvalQ(cl, {
    library(data.table)
  })

  my_scores <- parallel::parLapply(
    X = my_scores_list,
    cl = cl,
    fun = function(x) {
      temp <- lapply(x, function(y) {
        temp <- readRDS(y)
        temp <- data.table(temp[["slice_data"]])

        temp$img_id <- y

        tryCatch({
          temp$geometry <- do.call("c", temp$geometry)
        },
        error = function(e) browser())


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

  parallel::stopCluster(cl)

  # save scores <= 0.001
  my_scores_lte001 <- rbindlist(lapply(my_scores, function(x) x[["lte001"]]))
  my_scores_lte001$geometry <- st_sfc(my_scores_lte001$geometry)
  my_scores_lte001 <- st_sf(my_scores_lte001)

  saveRDS(my_scores_lte001,
          paste0(out_dir, "/nj-nearmap-scores-lte001_chunk", i, ".rds"))
  rm(my_scores_lte001)
  gc()

  # save scores > 0.001
  my_scores_gt001 <- rbindlist(lapply(my_scores, function(x) x[["gt001"]]))
  my_scores_gt001$geometry <- st_sfc(my_scores_gt001$geometry)
  my_scores_gt001 <- st_sf(my_scores_gt001)

  saveRDS(my_scores_gt001,
          paste0(out_dir, "/nj-nearmap-scores-gt001_chunk", i, ".rds"))
  rm(my_scores_gt001)
  rm(my_scores)
  gc()

  file_move(
    dir_ls(out_dir, type = "file"),
    "output/2019-05-28/nj-nearmap-scores"
  )

  file_delete(
    dir_ls("D:/wfu3/coolit.train/source_from-nj-nearmap-website/nj-nearmap-slices",
           type = "file")
  )
}

# collate scores
my_scores <- lapply(1:3, function(i) {
  data.table(
    readRDS(
      paste0(
        "output/2019-05-28/nj-nearmap-scores/nj-nearmap-scores-gt001_chunk", i, ".rds"
      )
    )
  )
})

my_scores <- rbindlist(my_scores)

saveRDS(my_scores,
        "output/2019-05-28/nj-nearmap-scores/nj-nearmap-scores-gt001.rds")
