library(coolit.train)
library(raster)
library(stringr)

img_dir <-  "data/source_from-ct-website/ct-images"

out_dir <- "data/curated-training-slices/ct/ct-slices"

img_file_names <- data.frame(
  img_file = list.files(img_dir, full.names = TRUE,
                        recursive = TRUE, pattern = "\\.jp2$"),
  stringsAsFactors = FALSE
)

img_file_names$stub <- str_match(img_file_names$img_file,
                                 "(.*/)*(.*)\\.jp2$")[, 3]

already_sliced <- list.files(out_dir, pattern = "\\.rds$")
already_sliced <- str_match(already_sliced, "^(.*)_slices\\.rds$")[, 2]

img_file_names <- img_file_names[!(img_file_names$stub %in% already_sliced), ]

images_to_slice <- split(img_file_names, 1:nrow(img_file_names))

cl <- parallel::makeCluster(40)
parallel::clusterEvalQ(cl, {
  library(coolit.train)
  library(raster)
  library(sf)
})
parallel::clusterExport(cl, c("out_dir"))

parallel::parLapplyLB(images_to_slice, cl = cl, fun = function(img) {

  temp_img <- brick(img[["img_file"]])

  if (nlayers(temp_img) > 3) {
    bonus_layers <- nlayers(temp_img) - 3

    bonus_layers <- rev(seq_len(bonus_layers) + 3)

    for (i in bonus_layers) {
      temp_img <- dropLayer(temp_img, i)
    }
  }

  try({
    slice_data <- slice_image(
      img_object = temp_img,
      slice_n_rows = 50,
      slice_n_cols = 50,
      slice_overlap = 0,
      complete_image = TRUE,
      verbose = FALSE
    )

    slice_data$img_id <- img[["stub"]]

    rm(temp_img)

    saveRDS(slice_data,
            file.path(out_dir, paste0(img[["stub"]], "_slices.rds")))

    rm(slice_data)
    gc()
  })
})


parallel::stopCluster(cl)
