library(sf)
library(stringr)
library(fs)

ct_index <- st_read("data/source_from-ct-website/ct-images/ct-image-index")

img_names <- str_match(ct_index$DESCRIPTIO, "(.*)\\.tif")[,2]

already_down <- dir_ls("data/source_from-ct-website/ct-images", type = "file")
already_down <- str_match(already_down, "(.*/)*(.*)\\.zip")[,3]

img_names <- img_names[!(img_names %in% already_down)]
img_names <- img_names[!is.na(img_names)]

for (i in seq_along(img_names)) {
  Sys.sleep(1)

  download.file(
    url = paste0(
      "http://www.cteco.uconn.edu/download/aerial/2016/tiles/sid4/",
      img_names[i],
      ".zip"),
    destfile = paste0("data/source_from-ct-website/ct-images/", img_names[i], ".zip"),
    mode = "wb",
    quiet = TRUE
  )
}



