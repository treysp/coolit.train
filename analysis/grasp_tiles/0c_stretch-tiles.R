library(raster)
library(parallel)
library(pbapply)

cores <- detectCores() - 1

inpath <- "C:/Users/wfu3/Desktop/temp/tiles/orig"
pixtable_outpath <- "C:/Users/wfu3/Desktop/temp/tiles/orig_pixtable"

# stretch files
rgb_stretch <- function(infile, outfile, stretch = c("lin", "hist"), write_only = TRUE) {
  my_brick <- brick(infile)
  
  if (length(stretch) != 1 || !(stretch %in% c("lin", "hist"))) {
    stop("parameter `stretch` must be one of 'lin' or 'hist'")
  }
    
  if (stretch == "hist") {
    stretch_fun <- raster:::.eqStretch
  } else if (stretch == "lin") {
    stretch_fun <- raster:::.linStretch
  }
  
  my_brick[[1]] <- stretch_fun(my_brick[[1]])
  my_brick[[2]] <- stretch_fun(my_brick[[2]])
  my_brick[[3]] <- stretch_fun(my_brick[[3]])
  
  out <- tryCatch({
    writeRaster(my_brick, filename = outfile, datatype = "INT1U", overwrite = TRUE)
    },
    warning = function(w) return(w),
    error = function(e) return(e)
    )
  
  if (write_only) {
    if (class(out) %in% c("warning", "error")) {
      out
    } else {
      TRUE
    }
  } else {
    out  
  }
}

## histogram stretch
orig_files <- list.files(inpath, recursive = TRUE)
new_files <- stringr::str_replace(orig_files, "\\.tif", "_rgb-hist-stretch.tif")

orig_files <- list.files(inpath, full.names = TRUE, recursive = TRUE)
new_files <- file.path(outpath, new_files)

cl <- makeCluster(cores)
clusterExport(cl, c("orig_files", "new_files", "rgb_stretch"))
clusterEvalQ(cl, {library(raster)})

out <- pblapply(cl = cl, X = seq_along(orig_files), FUN = function(i) {
  rgb_stretch(orig_files[i], new_files[i], stretch = "hist")
})

stopCluster(cl)

## linear stretch
orig_files <- list.files(inpath, recursive = TRUE)
new_files <- stringr::str_replace(orig_files, "\\.tif", "_rgb-lin-stretch.tif")

orig_files <- list.files(inpath, full.names = TRUE, recursive = TRUE)
new_files <- file.path(outpath, new_files)

cl <- makeCluster(cores)
clusterExport(cl, c("orig_files", "new_files", "rgb_stretch"))
clusterEvalQ(cl, {library(raster)})

out <- pblapply(cl = cl, X = seq_along(orig_files), FUN = function(i) {
  rgb_stretch(orig_files[i], new_files[i], stretch = "lin")
})

stopCluster(cl)
