library(raster)
library(stringr)
library(parallel)
library(pbapply)
source("util.R")

cores <- detectCores() - 1
write_only <- TRUE

base_path <- "c:/users/wfu3/desktop/temp/tiles/splits"

source_dirs <- c("orig", "rgb_hist-stretch", "rgb_lin-stretch")
dest_dirs <- c("grey-ave_orig", "grey-ave_hist-stretch", "grey-ave_lin-stretch")

out <- vector("list", length(source_dirs))
names(out) <- source_dirs

for (x in seq_along(source_dirs)) {
  files <- list.files(file.path(base_path, source_dirs[x]), 
                      full.names = TRUE, recursive = TRUE)
  files <- files[grepl("\\.tif$", files)]
  
  out_dirs <- str_match(files, "(.*?)/([^/]*?\\.tif)$")[, 2]
  out_dirs <- str_replace(out_dirs, 
                          paste0("/", source_dirs[x], "/"),
                          paste0("/", dest_dirs[x], "/")
                          )
  out_names <- str_match(files, "(.*?)/([^/]*?\\.tif)$")[, 3]
  out_names <- str_replace(out_names, "\\.tif", "_grey-ave\\.tif")
  out_paths <- fs::path(out_dirs, out_names)  
  
  if (length(files) != length(out_paths)) {
    stop("Problem with creation of destination paths from source paths.")
  }
  
  cl <- makeCluster(cores)
  clusterExport(cl, c("files", "out_paths", "rgb2grey", "write_only"))
  clusterEvalQ(cl, {library(raster)})
  
  out[[x]] <- pblapply(cl = cl, X = seq_along(files), function(y) {
    temp <- brick(files[y])
    
    temp <- rgb2grey(temp, method = "ave")
    
    out <- tryCatch({
      writeRaster(temp, filename = out_paths[y], datatype = "INT1U", overwrite = TRUE)
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
  })
  
  stopCluster(cl)
}

# # testing
# temp <- brick("C:/Users/wfu3/Desktop/temp/tiles/splits/orig/train/tower/980197_85_yes_tile23_yes.tif")
# temp_gamma <- temp
# 
# par(mfrow = c(3, 2))
# plotRGB(temp)
# plotRGB(stretch(temp))
# plot(rgb2grey(temp, "ave"), col = grey(0:255/255))
# plot(rgb2grey(stretch(temp), "ave"), col = grey(0:255/255))
# plot(rgb2grey(temp, "gleam"), col = grey(0:255/255))
# plot(rgb2grey(stretch(temp), "gleam"), col = grey(0:255/255))
# par(mfrow = c(1,1))


