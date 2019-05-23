library(sf)

my_bbox_poly <- st_zm(
  st_read("data/source_from-nj-nearmap-website/Union Co area of interest_2019-05-17/doc.kml")
)

my_bbox <- st_bbox(my_bbox_poly)

my_bbox_poly <- st_transform(my_bbox_poly, 3857)

# google maps tile zoom conversion to meters
# https://groups.google.com/forum/#!topic/google-maps-js-api-v3/hDRO4oHVSeM
#
# 156543.03392 * Math.cos(latLng.lat() * Math.PI / 180) / Math.pow(2, zoom)

calc_mt_per_px <- function(lat, zoom) {
  156543.03392 * cos(lat * pi / 180) / (2 ^ zoom)
}

img_resolution <- calc_mt_per_px(
  my_bbox[["ymin"]] + ((my_bbox[["ymax"]] - my_bbox[["ymin"]]) / 2),
  20
  )

grid_sq_size_mt <- img_resolution * 5000

# grid_sq_size_lat <- optimize(interval = c(0.006, 0.007),
#          f = function(x) {
#   out <- geosphere::distGeo(
#     c(my_bbox[["xmin"]], my_bbox[["ymin"]]),
#     c(my_bbox[["xmin"]] + x, my_bbox[["ymin"]])
#   )
#
#   grid_sq_size - out
# })$minimum

my_grid <- st_make_grid(my_bbox_poly, cellsize = grid_sq_size_lat)

my_grid <- st_transform(my_grid, 4326)

my_grid_bbox <- lapply(my_grid, function(x) {
  box <- st_bbox(x)

  box <- c(box[["ymin"]], box[["xmin"]], box[["ymax"]], box[["xmax"]])

  paste0(box, collapse = ",")
  })

my_grid_coord <- st_coordinates(my_grid)

my_grid_coord <- split(data.frame(my_grid_coord[, c("X", "Y")]), my_grid_coord[, "L2"])


temp <- raster::brick("c:/users/wfu3/desktop/staticmap.jpg")

for (i in seq(from = 0.005087372, to = 0.007, by = 0.00001)) {
  cat(i)
  print(geosphere::distGeo(
    c(my_bbox[["xmin"]], my_bbox[["ymin"]]),
    c(my_bbox[["xmin"]] + 0.00695052, my_bbox[["ymin"]])
  ))
  cat("\n")
}
