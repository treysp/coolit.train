library(stringr)
library(fs)
library(raster)
library(parallel)

# unzip source files
my_files <- list.files("F:/wfu3/coolit/data/source_from-philly-website",
                       full.names = TRUE)

sapply(my_files, function(x) {
  filename <- stringr::str_match(x, "(.*/)*(.*)\\.zip$")[,3]
  system(paste0("unzip ", x))

  dir.create(paste0("data/source_from-philly-website/", filename))
  mv_files <- list.files(".")[
    stringr::str_detect(list.files("."), filename)
    ]
  fs::file_move(mv_files, paste0("data/source_from-philly-website/", filename))
})

# downsample 3in/px to 6in/px
my_dirs <- dir("data/source_from-philly-website/3in", no.. = TRUE)

ncores <- detectCores() - 1

cl <- makeCluster(ncores)
clusterEvalQ(cl, {
  library(raster)
})

out <- pbapply::pblapply(X = my_dirs, cl = cl, FUN = function(x) {
temp <- tryCatch({
  brick(file.path("data/source_from-philly-website/3in",
                  x, paste0(x, ".tif")))
  },
  error = function(e) e
)

if (!("error" %in% class(temp))) {
  temp_agg <- aggregate(temp)

  writeRaster(temp_agg,
              file.path("data/source_from-philly-website/6in",
                        paste0(x, "_6in.tif")),
              datatype = "INT1U",
              overwrite = TRUE)
} else {
  return(temp)
}
})

stopCluster(cl)
