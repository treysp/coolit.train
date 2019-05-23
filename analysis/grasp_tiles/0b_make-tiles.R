library(raster)
library(parallel)
library(pbapply)
source("util.R")

inpath <- "C:/Users/wfu3/Desktop/temp/source"
outpath <- "C:/Users/wfu3/Desktop/temp/tiles"

cores <- detectCores() - 1

image_paths <- list.files(inpath, full.names = TRUE)

cl <- makeCluster(cores)
clusterExport(cl, c("outpath", "split_brick_px"))
clusterEvalQ(cl, {library(raster)})

split <- pblapply(cl = cl, X = image_paths, FUN = function(image) {
  brick <- brick(image)
  
  split_brick_px(brick, tile_w_px = 50L, tile_h_px = 50L, overlap = 10, 
                 path = outpath, write_only = TRUE, write_options = c())
})

stopCluster(cl)
