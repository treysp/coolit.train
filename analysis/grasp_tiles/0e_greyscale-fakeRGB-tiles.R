library(raster)
library(magick)
library(parallel)
library(pbapply)
library(stringr)

cores <- detectCores() - 10
write_only <- TRUE

base_path <- "data/tiles/splits/"

work_dir <- "working"

source_dirs <- c("grey-ave_orig", "grey-ave_hist-stretch", "grey-ave_lin-stretch")
dest_dirs <- c("grey-ave-fakeRGB_orig", "grey-ave-fakeRGB_hist-stretch", "grey-ave-fakeRGB_lin-stretch")

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
  out_names <- str_replace(out_names, "\\_grey-ave.tif", "_grey-ave-fakeRGB\\.png")
  out_paths <- fs::path(out_dirs, out_names)  
  
  if (length(files) != length(out_paths)) {
    stop("Problem with creation of destination paths from source paths.")
  }
  
  cl <- makeCluster(cores)
  clusterExport(cl, c("files", "out_paths", "work_dir", "write_only"))
  clusterEvalQ(cl, {library(raster); library(magick)})
  
  out[[x]] <- pblapply(cl = cl, X = seq_along(files), function(y) {
    temp <- raster(files[y])
    
    temp <- stack(temp, temp, temp)
    
    temp_file <- tempfile(pattern = "file", tmpdir = work_dir, fileext = ".tif")
    
    writeRaster(temp, 
                filename = temp_file, 
                datatype = "INT1U", overwrite = TRUE)
    
    temp <- image_read(temp_file)
    
    unlink(temp_file)
    
    out <- tryCatch({
      image_write(temp, out_paths[y], format = "png")
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
