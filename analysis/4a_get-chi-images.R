library(httr)

# pull from image server
lapply(1:5573, function(x) {
  Sys.sleep(3)

  out <- GET(
    paste0("https://gisimageserver.cookcountyil.gov/arcgis/rest/services/Cook2017/ImageServer/",
           x,
           "/info?f=pjson"),
    handle = handle("https://gisimageserver.cookcountyil.gov"))

  saveRDS(out,
          file.path("data/source_from-chi-website/json-info/",
                    paste0("chi-2017_img-info-", x, ".rds")))
})

# extract json from GET calls
chi <- lapply(list.files("data/source_from-chi-website/json-info/pages",
                         full.names = TRUE),
              readRDS)

table(sapply(temp, function(x) x$status_code))

chi_json <- lapply(chi, function(x) {
  jsonlite::fromJSON(httr::content(x))
})

saveRDS(chi_json, "data/source_from-chi-website/json-info/chi_json-info_2019-05-02.rds")

# extract image bounding boxes from json
chi_json <- readRDS("data/source_from-chi-website/json-info/chi_json-info_2019-05-02.rds")

chi_img_extent <- lapply(chi_json, function(x) x$extent)

chi_img_bbox <- lapply(chi_img_extent, function(x) {
  x_width <- x$xmax - x$xmin
  y_width <- x$ymax - x$ymin

  list(
    c(xmin = x$xmin,
      ymin = x$ymin,
      xmax = x$xmin + (x_width / 2),
      ymax = x$ymin + (y_width / 2)),
    c(xmin = x$xmin + (x_width / 2) + 1,
      ymin = x$ymin,
      xmax = x$xmax,
      ymax = x$ymin + (y_width / 2)),
    c(xmin = x$xmin,
      ymin = x$ymin + (y_width / 2) + 1,
      xmax = x$xmin + (x_width / 2),
      ymax = x$ymax),
    c(xmin = x$xmin + (x_width / 2) + 1,
      ymin = x$ymin + (y_width / 2) + 1,
      xmax = x$xmax,
      ymax = x$ymax)
  )
})

pbapply::pblapply(5474:5573, function(x) {
  boxes <- chi_img_bbox[[x]]

  for (i in seq_along(boxes)) {
    Sys.sleep(3)

    out <- GET(
      paste0(
        "https://gisimageserver.cookcountyil.gov/arcgis/rest/services/Cook2017/",
        "ImageServer/exportImage?bbox=",
        boxes[[i]][["xmin"]], "%2C",
        boxes[[i]][["ymin"]], "%2C",
        boxes[[i]][["xmax"]], "%2C",
        boxes[[i]][["ymax"]],
        "&bboxSR=&size=2500,2500&imageSR=&time=&format=png32&pixelType=U8&",
        "noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=",
        "+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=&",
        "mosaicRule=&renderingRule=&f=image"
        ),
      handle = handle("https://gisimageserver.cookcountyil.gov")
    )

    saveRDS(out,
            file.path("data/source_from-chi-website/get-responses",
                      paste0("chi_get-response_", x, "-", i, ".rds")))
  }
})

temp <- lapply(list.files("data/source_from-chi-website/get-responses", full.names = TRUE),
               readRDS)

temp <- lapply(temp, function(x) magick::image_read(content(x, "raw")))
