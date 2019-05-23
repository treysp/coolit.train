library(coolit)
library(raster)
library(stringr)
source("R/make_jp2_file_df.R")

jp2_dir <-  "data/source_from-nyc-website/nyc_ortho_jp2"
jp2_aux_dir <-  "data/source_from-nyc-website/nyc_ortho_jp2/"

out_dir <- "F:/coolit/output/2019-03-31/sliced_nyc"

tile_n_rows <- 50
tile_n_cols <- 50
tile_overlap <- 0
complete_image <- FALSE
verbose <- FALSE

jp2_file_names <- make_jp2_file_df(jp2_dir, jp2_aux_dir)
images_to_slice <- split(jp2_file_names, 1:nrow(jp2_file_names))

ncores <- parallel::detectCores() - 2
cl <- parallel::makeCluster(ncores)
parallel::clusterEvalQ(cl, {
  library(coolit)
})
parallel::clusterExport(
  cl,
  c("tile_n_rows", "tile_n_cols", "tile_overlap",
    "complete_image", "verbose", "out_dir"))

pbapply::pblapply(images_to_slice, cl = cl, FUN = function(img) {

  tile_data <- slice_jp2_image(
    jp2_path = img[["img_file"]],
    jp2_aux_path = img[["aux_file"]],
    tile_n_rows = tile_n_rows,
    tile_n_cols = tile_n_cols,
    tile_overlap = tile_overlap,
    complete_image = complete_image,
    verbose = verbose
  )

  saveRDS(tile_data,
          file.path(out_dir, paste0(img[["stub"]], "_slices.rds")),
          compress = FALSE)
})


parallel::stopCluster(cl)
