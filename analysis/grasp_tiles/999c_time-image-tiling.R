library(raster)
library(sf)
library(magick)
library(pbapply)
library(tidyr)
source("util.R")

in_name <- "980195.tif"
inpath <- "C:/Users/wfu3/Desktop/temp"
outpath <- "C:/Users/wfu3/Desktop/temp/working"

# # time splitting with raster package: 2h40m
# brick <- brick(file.path(inpath, in_name))
# 
# start_raster <-  Sys.time()
# split_brick_px(brick, tile_w_px = 2500L, tile_h_px = 2500L, overlap = 0,
#                path = outpath, write_only = TRUE, write_options = c())
# end_raster <- Sys.time()
# (end_raster - start_raster)

# # time splitting with magick package: 56s
# start_raster <-  Sys.time()
# source_image <- image_read(file.path(inpath, in_name))
# source_image <- image_read(image_data(source_image)[1:3,,]) # drop 4th channel
# source_image_info <- image_info(source_image)
# 
# out_name <- stringr::str_replace(in_name, "\\.tif", "")
# 
# tile_corners <- calc_tile_corners(
#   source_n_rows = source_image_info$height,
#   source_n_cols = source_image_info$width,
#   tile_n_rows = 50,
#   tile_n_cols = 50
# )
# 
# img_crop_geometry <- split(tile_corners, 1:NROW(tile_corners))
# img_crop_geometry <- lapply(img_crop_geometry, function(x) {
#   geometry_area(
#     width = length(x$x0[1]:x$x1[1]),
#     height = length(x$y0[1]:x$y1[1]),
#     x_off = x$x0[1] - 1,
#     y_off = x$y0[1] - 1)
# })
# 
# pblapply(seq_along(img_crop_geometry), function(i) {
#   out <- image_crop(image = source_image,
#                     geometry = img_crop_geometry[[i]])
# 
#   image_write(out,
#               path = file.path(outpath, paste0(out_name, "_tile", i, ".tif")))
# 
#   NULL
# })
# end_raster <- Sys.time()
# (end_raster - start_raster)

start <- Sys.time()
# never write images to file - slice up array and pass to model for scoring
# read jp2 directly
jp2_xml <- xml2::as_list(
  xml2::read_xml("E:/tower-id/data/source_from-nyc-website/boro_manhattan_sp16/980195.jp2.aux.xml")
)

source_brick <- brick(
  "E:/tower-id/data/source_from-nyc-website/boro_manhattan_sp16/980195.jp2",
  crs = st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string
)

source_brick <- dropLayer(source_brick, 4)

# create raster extents and sf polygons for each tile
tile_data <- calc_tile_corners(
  source_n_rows = nrow(source_brick),
  source_n_cols = ncol(source_brick),
  tile_n_rows = 50,
  tile_n_cols = 50
)

tile_data <- cbind(source_img = "E:/tower-id/data/source_from-nyc-website/boro_manhattan_sp16/980195.jp2",
                   source_img_aux = "E:/tower-id/data/source_from-nyc-website/boro_manhattan_sp16/980195.jp2.aux.xml",
                   tile_id = seq_len(nrow(tile_data)), 
                   tile_data,
                   stringsAsFactors = FALSE)

tile_extents <- split(tile_data, 1:NROW(tile_data))

tile_data$extents <- pblapply(tile_extents, function(x) {
  extent(source_brick, x[["y0"]], x[["y1"]], x[["x0"]], x[["x1"]])
})

tile_data$geometry <- pblapply(tile_data$extents, function(x) {
  st_sfc(
    st_polygon(
      list(
        rbind(
          c(x@xmin, x@ymin), 
          c(x@xmin, x@ymax), 
          c(x@xmax, x@ymax), 
          c(x@xmax, x@ymin), 
          c(x@xmin, x@ymin) 
        )
      )
    ),
    crs = crs(source_brick)@projargs
  )
})

# create a 4d array for each tile, containing 1 [50, 50, 3] slice
source_brick_data <- as.array(source_brick)

tile_data$tile_array <- pblapply(1:nrow(tile_data), function(i) { 
  out <- array(NA, c(1, 50, 50, 3))
  
  out[1, , ,] <- source_brick_data[
    seq(tile_data[["x0"]][i], tile_data[["x1"]][i]),
    seq(tile_data[["y0"]][i], tile_data[["y1"]][i])
    ,
    ]
  
  out
  })

# # to reassemble original brick
# temp <- crop(source_brick, tile_data$extents[[1]])
# plotRGB(temp)
# 
# temp_array <- drop(tile_data$tile_array[[1]])
# temp_array[,,1] <- t(temp_array[,,1])
# temp_array[,,2] <- t(temp_array[,,2])
# temp_array[,,3] <- t(temp_array[,,3])
# plotRGB(temp)

# bind image arrays for scoring with model
tile_array <- abind::abind(tile_data$tile_array, along = 1)
Sys.time() - start