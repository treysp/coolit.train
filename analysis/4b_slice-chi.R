library(coolit)
library(magick)
library(raster)
library(sf)

out_dir <- "data/source_from-chi-website/chi-slices"

my_json <- readRDS(
  "data/source_from-chi-website/json-info/chi_json-info_2019-05-02.rds"
  )

my_get_paths <- fs::dir_ls("data/source_from-chi-website/get-responses",
                           type = "file")

my_img_nums <- as.numeric(
    stringr::str_match(my_get_paths, "(.*/).*_.*_(\\d{1,4}).*")[, 3]
  )

img_num_count <- table(my_img_nums)
my_img_nums <- as.numeric(names(img_num_count)[img_num_count == 4])

already_sliced <- fs::dir_ls("data/source_from-chi-website/chi-slices",
                             type = "file")

already_sliced <- as.numeric(
  stringr::str_match(already_sliced, "(.*/)(\\d{1,4})_slices\\.rds")[,3]
)

my_img_nums <- my_img_nums[!(my_img_nums %in% already_sliced)]

images_to_slice <- split(my_img_nums, 1:length(my_img_nums))

ncores <- parallel::detectCores() - 3
cl <- parallel::makeCluster(ncores)
parallel::clusterEvalQ(cl, {
  library(coolit)
  library(magick)
  library(httr)
  library(sf)
  library(raster)
})
parallel::clusterExport(cl, c("out_dir", "my_json"))

pbapply::pblapply(images_to_slice, cl = cl, FUN = function(img_num) {
  my_json <- my_json[[img_num]]

  my_extent <- my_json$extent[1:4]

  x_width <- my_extent$xmax - my_extent$xmin
  y_width <- my_extent$ymax - my_extent$ymin

  my_extents <- list(
    c(xmin = my_extent$xmin,
      ymin = my_extent$ymin,
      xmax = my_extent$xmin + (x_width / 2),
      ymax = my_extent$ymin + (y_width / 2)),
    c(xmin = my_extent$xmin + (x_width / 2) + 1,
      ymin = my_extent$ymin,
      xmax = my_extent$xmax,
      ymax = my_extent$ymin + (y_width / 2)),
    c(xmin = my_extent$xmin,
      ymin = my_extent$ymin + (y_width / 2) + 1,
      xmax = my_extent$xmin + (x_width / 2),
      ymax = my_extent$ymax),
    c(xmin = my_extent$xmin + (x_width / 2) + 1,
      ymin = my_extent$ymin + (y_width / 2) + 1,
      xmax = my_extent$xmax,
      ymax = my_extent$ymax)
  )

  my_slices <- lapply(1:4, function(x) {
    my_get <- readRDS(
      file.path("data/source_from-chi-website/get-responses",
                paste0("chi_get-response_", img_num, "-", x, ".rds"))
    )

    get_response_to_array <- function(response) {
      out <- magick::image_read(httr::content(response, "raw"))
      out <- as.integer(magick::image_data(out, "rgba"))
      out <- out[,,1:3]
      out
    }

    img_array <- get_response_to_array(my_get)

    img_brick <- brick(
      img_array,
      xmn = my_extents[[x]][["xmin"]],
      xmx = my_extents[[x]][["xmax"]],
      ymn = my_extents[[x]][["ymin"]],
      ymx = my_extents[[x]][["ymax"]],
      crs = sf::st_crs(wkt = my_json$extent$spatialReference$wkt)$proj4string
      )

    out <- slice_image(
      img_object = img_brick,
      slice_n_rows = 50,
      slice_n_cols = 50,
      complete_image = TRUE
    )

    out$slice_id <- out$slice_id + nrow(out) * (x - 1)

    out
  })

  my_slices <- do.call("rbind", my_slices)

  my_slices$geometry <- do.call("c", my_slices$geometry)

  my_slices <- st_sf(my_slices)

  saveRDS(my_slices,
          file.path(out_dir,
                    paste0(img_num, "_slices.rds")),
          compress = FALSE)
})

parallel::stopCluster(cl)

# combine slices to create full-size images
my_proj <- readRDS("data/source_from-chi-website/json-info/chi_json-info_2019-05-02.rds")
my_proj <- sf::st_crs(wkt = my_proj[[1]]$extent$spatialReference$wkt)$proj4string

already_made <- fs::dir_ls("data/source_from-chi-website/chi-images", type = "file")
already_made <- as.numeric(
  stringr::str_match(already_made, "(.*/)(\\d{1,4}).*")[, 3]
)

to_make <- seq_len(
  length(fs::dir_ls("data/source_from-chi-website/chi-slices", type = "file"))
  )

to_make <- to_make[!(to_make %in% already_made)]

ncores <- parallel::detectCores() - 10
cl <- parallel::makeCluster(ncores)
parallel::clusterEvalQ(cl, {
  library(coolit)
  library(magick)
  library(sf)
  library(raster)
})
parallel::clusterExport(cl, c("my_proj"))

out <- parallel::parLapplyLB(X = to_make, cl = cl, fun = function(i) {
  temp_slices <- readRDS(
    paste0("data/source_from-chi-website/chi-slices/", i, "_slices.rds")
  )

  tryCatch({
    temp_bricks <- lapply(1:nrow(temp_slices), function(j) {
      out <- brick(drop(temp_slices$slice_array[[j]]))

      extent(out) <- temp_slices$extents[[j]]

      crs(out) <- my_proj

      res(out) <-  c(0.5, 0.5)

      origin(out) <- c(0, 0)

      out
    })

    temp_img <- do.call("merge", temp_bricks)

    magick::image_write(magick::image_read(as.array(temp_img) / 255),
                        paste0("data/source_from-chi-website/chi-images/", i, ".png"))

    rm(temp_slices)
    rm(temp_bricks)
    rm(temp_img)
  },
  error = function(e) e)
})

parallel::stopCluster(cl)
