# lead building footprint shape files
# calculate
#   - proportion tile area that is building footprint
#   - total number building
#   - number buildings / area
#

library(sf)
library(data.table)
library(parallel)

# read msft data
msft_bldg_orig <- fread("data/MSFT_NewYork/nyc_microsoft_orig.csv",
                        sep = "")
names(msft_bldg_orig) <- "wkt"
msft_bldg_orig[, wkt := gsub('"', "", wkt)]

ncores <- detectCores() - 1
partition_len <- ceiling(nrow(msft_bldg_orig) / ncores)
partitions <- rep(1:ncores, each = partition_len)
partitions <- partitions[1:nrow(msft_bldg_orig)]

msft_bldg_orig[, partition := partitions]
msft_bldg_list <- split(msft_bldg_orig, by = "partition")

cl <- makeCluster(ncores)
clusterEvalQ(cl, {
  library(data.table)
  library(sf)
})

msft_bldg_list <- parLapply(
  cl = cl,
  X = msft_bldg_list,
  fun = function(x) {
    x[, geometry := st_as_sfc(wkt, crs = 4326)]
    x[, valid := st_is_valid(geometry)]

    list(valid = x[valid == TRUE, geometry, drop = TRUE],
         invalid = x[valid == FALSE, geometry, drop = TRUE])
  })

stopCluster(cl)

msft_bldg <- lapply(msft_bldg_list,
                    function(x) x[["valid"]])

msft_bldg <- do.call("c", msft_bldg)

# msft_bldg_geom <- msft_bldg$geometry[msft_bldg[valid == TRUE, which = TRUE]]
# msft_bldg_geom_bad <- st_is_valid(msft_bldg_geom)
#
# msft_bldg_bad <- msft_bldg$geometry[msft_bldg[valid == FALSE, which = TRUE]]
# msft_bldg_bad <- lwgeom::st_make_valid(msft_bldg_bad)
#
# msft_bldg_geom <- c(msft_bldg_geom, msft_bldg_bad)

nyc_towers_wgs84 <- lwgeom::st_transform_proj(nyc_towers, crs = 4326)

nyc_bldg <- st_crop(msft_bldg, st_bbox(nyc_towers_wgs84))

my_bbox <- st_bbox(nyc_towers_wgs84)
my_xmin <- my_bbox["xmin"] + .37 * (my_bbox["xmax"] - my_bbox["xmin"])
my_xmax <- my_bbox["xmax"] - .53 * (my_bbox["xmax"] - my_bbox["xmin"])

my_ymin <- my_bbox["ymin"] + .48 * (my_bbox["ymax"] - my_bbox["ymin"])
my_ymax <- my_bbox["ymax"] - .42 * (my_bbox["ymax"] - my_bbox["ymin"])


plot(st_crop(x = nyc_bldg,
             y = st_bbox(c(my_xmin, my_ymin, my_xmax, my_ymax),
               crs = st_crs(nyc_towers_wgs84)
               )
             )
  )

plot(st_centroid(nyc_towers_wgs84$geometry), add = TRUE,
     pch = 19, col = "red", cex = .2)
