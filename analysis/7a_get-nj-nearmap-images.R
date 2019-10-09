library(sf)

my_bbox_poly <- st_zm(
  st_read("data/source_from-nj-nearmap-website/Union Co area of interest_2019-05-17/doc.kml")
)

my_bbox <- st_bbox(my_bbox_poly)


zoom_resolution_meters <- function(lat, zoom) {
  156543.03392 * cos(lat * pi / 180) / (2 ^ zoom)
}

# steps
#  - convert top corner from lat/long to meters
#  - add 5000 * res in [x, y] directions to calculate corners in meters
#  - convert corners back to lat/long
my_res <- zoom_resolution_meters(my_bbox[["ymax"]], 20)
my_box_size <- my_res * 5000

my_bbox_poly_meters <- st_transform(my_bbox_poly, 900913)

my_grid <- st_make_grid(my_bbox_poly_meters, cellsize = my_box_size)

my_grid_lonlat <- st_transform(my_grid, 4326)

my_grid_bbox <- lapply(my_grid_lonlat, function(x) {
  box <- st_bbox(x)

  box <- c(box[["ymin"]], box[["xmin"]], box[["ymax"]], box[["xmax"]])

  paste0(box, collapse = ",")
  })

names(my_grid_bbox) <- seq_along(my_grid_bbox)

saveRDS(my_grid_bbox,
        "data/source_from-nj-nearmap-website/nj-nearmap-images/img_bboxes_2019-05-24.rds"
        )

already_down <- list.files("data/source_from-nj-nearmap-website/nj-nearmap-images/",
                           pattern = ".jpg")
already_down <- as.numeric(gsub("\\.jpg", "", already_down))

to_down <- seq_along(my_grid_bbox)[!(seq_along(my_grid_bbox) %in% already_down)]

lapply(to_down, function(i) {
  Sys.sleep(1)

  download.file(
    url = paste0("http://us0.nearmap.com/staticmap?bbox=",
                 my_grid_bbox[[i]],
                 "&zoom=20&date=20190523&httpauth=false&",
                 "apikey=MYAPIKEY"),
    destfile = paste0("data/source_from-nj-nearmap-website/nj-nearmap-images/", i, ".jpg"),
    mode = "wb",
    quiet = TRUE
  )
})

