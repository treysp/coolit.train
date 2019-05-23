library(coolit)
library(raster)
library(stringr)

img_dir <-  "data/source_from-philly-website/6in"

out_dir <- "F:/wfu3/coolit/output/2019-04-18/sliced_philly"

img_file_names <- data.frame(
  img_file = list.files(img_dir, full.names = TRUE,
                        recursive = TRUE, pattern = "\\.tif$"),
  stringsAsFactors = FALSE
)

img_file_names$stub <- str_match(img_file_names$img_file,
                                 "(.*/)*(.*)\\.tif$")[, 3]

images_to_slice <- split(img_file_names, 1:nrow(img_file_names))

ncores <- parallel::detectCores() - 2
cl <- parallel::makeCluster(ncores)
parallel::clusterEvalQ(cl, {
  library(coolit)
})
parallel::clusterExport(cl, "out_dir")

pbapply::pblapply(images_to_slice, cl = cl, FUN = function(img) {

  slice_data <- slice_image(
    img_path = img[["img_file"]],
    slice_n_rows = 50,
    slice_n_cols = 50,
    slice_overlap = 0,
    complete_image = TRUE,
    verbose = FALSE
  )

  saveRDS(slice_data,
          file.path(out_dir, paste0(img[["stub"]], "_slices.rds")),
          compress = FALSE)
})


parallel::stopCluster(cl)
