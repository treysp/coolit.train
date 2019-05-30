library(coolit.train)
library(raster)
library(stringr)

my_proj4 <- sf::st_crs(4326)$proj4string

img_bbox <- readRDS(
  "d:/wfu3/coolit.train/source_from-nj-nearmap-website/nj-nearmap-images/img_bboxes_2019-05-24.rds"
  )

img_dir <-  "d:/wfu3/coolit.train/source_from-nj-nearmap-website/nj-nearmap-images"

out_dir <- "d:/wfu3/coolit.train/source_from-nj-nearmap-website/nj-nearmap-slices"

img_file_names <- data.frame(
  img_file = list.files(img_dir, full.names = TRUE,
                        recursive = TRUE, pattern = "\\.jpg$"),
  stringsAsFactors = FALSE
)

img_file_names$stub <- str_match(img_file_names$img_file,
                                 "(.*/)*(.*)\\.jpg$")[, 3]

already_sliced <- list.files(out_dir, pattern = "\\.rds$")
already_sliced <- str_match(already_sliced, "^(\\d{1,4}).*")[, 2]

img_file_names <- img_file_names[!(img_file_names$stub %in% already_sliced), ]

images_to_slice <- split(img_file_names, 1:nrow(img_file_names))

ncores <- parallel::detectCores() - 21
cl <- parallel::makeCluster(ncores)
parallel::clusterEvalQ(cl, {
  library(coolit.train)
  library(raster)
  library(sf)
})
parallel::clusterExport(cl, c("out_dir", "my_proj4", "img_bbox"))

parallel::parLapplyLB(images_to_slice, cl = cl, fun = function(img) {
  temp_img_bbox <- as.numeric(strsplit(img_bbox[[img[["stub"]]]], ",")[[1]])

  temp_img <- brick(img[["img_file"]], crs = my_proj4)

  extent(temp_img) <- extent(
    temp_img_bbox[2],
    temp_img_bbox[4],
    temp_img_bbox[1],
    temp_img_bbox[3]
    )

  temp_img <- projectRaster(temp_img, crs = st_crs(3857)$proj4string)

  temp_img[[1]] <- calc(temp_img[[1]], function(x) {
    x[x < 0] <- 0
    x[x > 255] <- 255
    x
    })

  temp_img[[2]] <- calc(temp_img[[2]], function(x) {
    x[x < 0] <- 0
    x[x > 255] <- 255
    x
    })

  temp_img[[3]] <- calc(temp_img[[3]], function(x) {
    x[x < 0] <- 0
    x[x > 255] <- 255
    x
    })

  slice_data <- slice_image(
    img_object = temp_img,
    slice_n_rows = 50,
    slice_n_cols = 50,
    slice_overlap = 0,
    complete_image = TRUE,
    verbose = FALSE
  )

  rm(temp_img)

  saveRDS(slice_data,
          file.path(out_dir, paste0(img[["stub"]], "_slices.rds")),
          compress = FALSE)

  rm(slice_data)
  gc()
})


parallel::stopCluster(cl)
