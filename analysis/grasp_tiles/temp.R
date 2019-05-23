library(dplyr)
library(stringr)
library(magick)
library(sf)
library(raster)

source_image_dir <- "E:/tower-id/data/source_5000px/980195.tif"
source_image <- image_read(source_image_dir)
source_raster <- raster::brick(source_image_dir)
source_raster <- dropLayer(source_raster, 4)

small_tile_dir <- c("E:/tower-id/data/tiles/splits/orig/test/tower",
                    "E:/tower-id/data/tiles/splits/orig/train/tower",
                    "E:/tower-id/data/tiles/splits/orig/validate/tower")
  
small_tile_dim <- 50
small_tile_overlap <- 10

mid_tile_dim <- 250
mid_tile_overlap <- 0

source_image_dim <- image_info(source_image)$width

small_tile_files <- unlist(lapply(small_tile_dir, list.files))
small_tile_files <- small_tile_files[str_detect(small_tile_files, "980195")]

file_name_regex <- "(\\d{6})_(\\d{2,3})_(yes)?_?tile(\\d{1,2})_?(yes)?\\.png"

small_tiles_orig <- data.frame(str_match(small_tile_files, file_name_regex),
                stringsAsFactors = FALSE)

names(small_tiles_orig) <- c("file_name", "source_img_id", 
                             "mid_tile_id", "mid_tile_tower", 
                        "small_tile_id", "small_tile_tower")

# TODO: check conformity of tile size, overlap, parent tile size

# num small tiles in each mid tile
num_small_per_mid_1d <- mid_tile_dim %/% 
  (small_tile_dim - small_tile_overlap)

num_mid_per_source_1d <- source_image_dim %/% 
  (mid_tile_dim - mid_tile_overlap)

# create grid for mid tiles
mid_grid_0 <-  (seq(0, (num_mid_per_source_1d - 1)) * mid_tile_dim) + 1
mid_grid_1 <-  (seq(1, (num_mid_per_source_1d)) * mid_tile_dim) 
mid_grid <- data.frame(x0 = mid_grid_0, x1 = mid_grid_1, 
                       y0 = mid_grid_0, y1 = mid_grid_1)
mid_grid <- tidyr::expand(
  mid_grid, 
  tidyr::nesting(x0, x1), 
  tidyr::nesting(y0, y1)
  )

mid_grid_list <- split(mid_grid, 1:NROW(mid_grid))

mid_grid_list <- lapply(mid_grid_list, function(x) {
  list(
    rbind(
      c(x$x0[1], x$y0[1]),
      c(x$x1[1], x$y0[1]),
      c(x$x1[1], x$y1[1]),
      c(x$x0[1], x$y1[1]),
      c(x$x0[1], x$y0[1])
    )
  )
})

mid_grid_multipolygon <- st_multipolygon(mid_grid_list)

# create grid for small tiles
small_grid_0 <-  (seq(0, (num_small_per_mid_1d - 1)) * (small_tile_dim - small_tile_overlap)) + 1
small_grid_1 <-  small_grid_0 + small_tile_dim - 1
small_grid <- data.frame(x0_small = small_grid_0, x1_small = small_grid_1, 
                         y0_small = small_grid_0, y1_small = small_grid_1)
small_grid <- tidyr::expand(
  small_grid, 
  tidyr::nesting(x0_small, x1_small), 
  tidyr::nesting(y0_small, y1_small)
)

mid_grid$mid_tile_num_x <- ((1:nrow(mid_grid) - 1) %/% length(mid_grid_0))
mid_grid$mid_tile_num_y <- ((1:nrow(mid_grid) - 1) %% length(mid_grid_0))

small_grid_orig <- cbind(
  mid_grid[rep(1:nrow(mid_grid), each = nrow(small_grid)),],
  small_grid[rep(1:nrow(small_grid), nrow(mid_grid)), ]
)

small_grid <- as_tibble(small_grid_orig) %>% 
  rename(x0_mid = x0, x1_mid = x1, y0_mid = y0, y1_mid = y1) %>% 
  mutate(
    x0_small = x0_small + mid_tile_num_x * mid_tile_dim,
    x1_small = x1_small + mid_tile_num_x * mid_tile_dim,
    y0_small = y0_small + mid_tile_num_y * mid_tile_dim,
    y1_small = y1_small + mid_tile_num_y * mid_tile_dim
  ) %>%
  dplyr::select(ends_with("_small"))

names(small_grid) <- stringr::str_replace(names(small_grid), "_small", "")

small_grid_list <- split(small_grid, 1:NROW(small_grid))

small_grid_list <- lapply(small_grid_list, function(x) {
  list(
    rbind(
      c(x$x0[1], x$y0[1]),
      c(x$x1[1], x$y0[1]),
      c(x$x1[1], x$y1[1]),
      c(x$x0[1], x$y1[1]),
      c(x$x0[1], x$y0[1])
    )
  )
})

small_grid_multipolygon <- st_multipolygon(small_grid_list)

# tile locations
small_tiles <- small_tiles_orig %>% 
  mutate(
    source_img_id = as.numeric(source_img_id),
    mid_tile_id = as.numeric(mid_tile_id) - 1,
    mid_tile_tower = ifelse(mid_tile_tower == "yes", 1, 0),
    small_tile_id = as.numeric(small_tile_id) - 1,
    small_tile_tower = ifelse(small_tile_tower == "yes", 1, 0),
    
    mid_tile_num_y = mid_tile_id %/% num_mid_per_source_1d,
    mid_tile_num_x = mid_tile_id %% num_mid_per_source_1d,
    
    mid_tile_y0 = mid_tile_num_y * (mid_tile_dim - mid_tile_overlap),
    mid_tile_y1 = mid_tile_y0 + mid_tile_dim,
    
    mid_tile_x0 = mid_tile_num_x * (mid_tile_dim - mid_tile_overlap),
    mid_tile_x1 = mid_tile_x0 + mid_tile_dim,
    
    small_tile_num_y = small_tile_id %/% num_small_per_mid_1d,
    small_tile_num_x = small_tile_id %% num_small_per_mid_1d + 1,
    
    small_tile_y0 = small_tile_num_y * (small_tile_dim - small_tile_overlap),
    small_tile_y1 = small_tile_y0 + small_tile_dim,
    
    small_tile_x0 = small_tile_num_x * (small_tile_dim - small_tile_overlap),
    small_tile_x1 = small_tile_x0 + small_tile_dim,
    
    poly_y0 = mid_tile_y0 + small_tile_y0 + 1,
    poly_y1 = mid_tile_y0 + small_tile_y1,
    
    poly_x0 = mid_tile_x0 + small_tile_x0 + 1,
    poly_x1 = mid_tile_x0 + small_tile_x1,
    
    poly_ref_y1 = poly_y1 + ymin(source_raster),
    poly_ref_y0 = poly_y0 + ymin(source_raster),
    
    poly_ref_x0 = poly_x0 + xmin(source_raster),
    poly_ref_x1 = poly_x1 + xmin(source_raster)
  ) %>% 
  arrange(mid_tile_id, small_tile_id)

poly_list <- split(small_tiles, small_tiles$mid_tile_id)

poly_list <- lapply(poly_list, function(x) {
  out <- vector(mode = "list", nrow(x))
  
  for (i in seq(1, nrow(x))) {
    out[[i]] <- st_polygon(
      list(
        rbind(
            c(x$poly_x0[i], -1 * x$poly_y0[i] + 5000 + 1),
            c(x$poly_x1[i], -1 * x$poly_y0[i] + 5000 + 1),
            c(x$poly_x1[i], -1 * x$poly_y1[i] + 5000 + 1),
            c(x$poly_x0[i], -1 * x$poly_y1[i] + 5000 + 1),
            c(x$poly_x0[i], -1 * x$poly_y0[i] + 5000 + 1)
          )
      )
    )
  }
  
  out
})

#### NOTES 3-9-2019 ###########
# the following "works" in the sense that it will rasterize the sf polygons
#   and add them to the source_raster.
# even though the poly_set has the same projection, it's unclear to
#   me how to go from raw cell coordinates to the projection of the source_raster
#   correctly
##############################
poly_set <- st_union(
  st_sfc(
    purrr::flatten((poly_list)), 
    crs = crs(source_raster)@projargs
    )
  )

poly_set <- st_sf(geometry = st_cast(poly_set, "POLYGON"))

### if we create `poly_set` without a projection this plot gets very close
temp <- matrix(values(source_raster[[1]]), nrow = 5000, byrow = TRUE)
temp <- reshape2::melt(temp)
names(temp) <- c("y", "x", "value")
temp$y <- -1 * temp$y + 5000 + 1
ggplot() + 
  geom_raster(data = temp, aes(x = x, y = y, fill = value)) + 
  geom_sf(data = poly_set, fill = NA, col = "red") + 
  theme_minimal()
#####################################################################

poly_raster <- fasterize::fasterize(poly_set, 
                                    raster = raster(poly_set,
                                                    nrow = 5000, ncol = 5000)
                                    )
poly_raster <- calc(poly_raster, function(x) x * 255)
extent(poly_raster) <- extent(source_raster)

source_raster <- addLayer(source_raster, source_raster[[1]])
source_raster <- setValues(source_raster, values = getValues(poly_raster), layer = 4)

source_raster[[1]] <- mosaic(source_raster[[1]], source_raster[[4]], fun = max)
source_raster <- dropLayer(source_raster, 4)
plotRGB(source_raster)
