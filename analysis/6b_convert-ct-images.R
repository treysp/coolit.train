my_zips <- list.files(
  "f:/wfu3/coolit.train/data/source_from-ct-website/ct-images-zips",
  full.names = TRUE,
  pattern = "zip"
  )

cl <- parallel::makeCluster(
  parallel::detectCores() - 1
)
parallel::clusterEvalQ(cl, {
  library(rgdal)
  library(raster)
  library(stringr)
  library(gdalUtils)
  library(fs)
})

parallel::parLapplyLB(my_zips, cl = cl, fun = function(my_zip) {

  my_dir <- str_replace(my_zip, "ct-images-zips", "ct-images")
  my_dir <- str_replace(my_dir, "\\.zip", "")
  my_image <- str_match(my_dir, "(.*/)*(.*)$")[, 3]

  unzip(my_zip, exdir = my_dir)

  my_dir_win <- str_replace_all(my_dir, "/", "\\\\")

  shell(
    paste0("F:\\wfu3\\mrsid_decode\\raster_cli\\mrsiddecode.exe ",
           "-i ", my_dir_win, "\\", my_image, ".sid ",
           "-o ", my_dir_win, "\\", my_image, ".tif ",
           "-of tifg")
  )

  gdalUtils::gdal_translate(
    src_dataset = paste0(my_dir, "/", my_image, ".tif"),
    dst_dataset = paste0(my_dir, "/", my_image, ".jp2"),
    of = "JP2OpenJPEG"
  )

  fs::file_delete(paste0(my_dir, "/", my_image, ".tif"))
  fs::file_delete(paste0(my_dir, "/", my_image, ".sid"))

})

parallel::stopCluster(cl)
rm(cl)
