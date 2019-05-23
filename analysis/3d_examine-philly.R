library(sf)
library(raster)
library(mapview)
library(magick)

# get image crs
philly_crs <- brick(
  "data/source_from-philly-website/6in/26453E204934N_6in.tif"
)
philly_crs <- crs(philly_crs)@projargs

# get scores
scores <- readRDS("output/2019-04-18/scored_philly/philly-scores-gt0338-lte1.rds")

tower_coord_buff <- st_buffer(tower_coord$geometry, 25)

slice_tower_intersect <- st_intersects(scores, tower_coord_buff)



# create tower coords sf
tower_coord <- readxl::read_excel(
  "data/philly_tower-coords_grasp/PHLCTList_02082019.xlsx"
)
names(tower_coord) <- tolower(names(tower_coord))

tower_coord <- st_as_sf(tower_coord,
                        coords = c("pointy", "pointx"),
                        agr = "constant")
st_crs(tower_coord) <- 4326

tower_coord <- st_transform(tower_coord, crs = st_crs(scores))

# get img index
philly_index <- st_read("data/source_from-philly-website/PhiladelphiaImagery2018")
philly_index <- st_transform(philly_index, crs = st_crs(scores))

philly_intersect <- st_intersects(philly_index, tower_coord)

philly_index$hastower_coord <- sapply(philly_intersect, function(x) length(x) != 0)

# make plots
make_tower_plot <- function(tower_img_name, scores_sf, tower_coord_sf = NULL, outpath) {
  tower_img <- brick(tower_img_name)

  out_name <- stringr::str_match(tower_img_name,
                                 "(.*/)*(.*)\\.(tif|png|jp2)")[, 3]

  png(file.path(outpath, paste0(out_name, "_tower-plot.png")),
      width = ncol(tower_img), height = nrow(tower_img))

  plotRGB(tower_img, maxpixels = 1e10)

  plot(
    scores_sf$geometry[scores_sf$source_img == tower_img_name],
    col = scales::alpha("red", .4),
    add = TRUE
  )

  if (!is.null(tower_coord_sf)) {
    tower_coord_img <- st_crop(tower_coord, st_bbox(tower_img))

    plot(
      st_buffer(tower_coord_img$geometry, 25),
      col = scales::alpha("blue", .4),
      add = TRUE
    )
  }

    dev.off()
}

# subset scores and identify source images that contain a slice
plot_scores <- scores[scores$predicted_probs >= 0.8,]

philly_intersect <- st_intersects(philly_index, plot_scores)

philly_index$hastower_score <- sapply(philly_intersect, function(x) length(x) != 0)

# make vector of image names: those in plot_scores and those
#  with a coord tower but not in plot_scores
plot_img_names <- unique(plot_scores$source_img)

img_path <- stringr::str_match(plot_img_names[1], "(.*/)*(.*)\\.(tif|png|jp2)")[,2]

coord_img_names <- philly_index[philly_index$hastower_coord &
                                  !philly_index$hastower_score,
                                "Name"]
coord_img_names <- paste0(img_path, coord_img_names$Name, "_6in.tif")

plot_img_names <- c(plot_img_names, coord_img_names)

cl <- parallel::makeCluster(parallel::detectCores() - 1)

parallel::clusterEvalQ(cl, {
  library(raster)
  library(stringr)
  library(sf)
  library(scales)
})

parallel::clusterExport(cl, c("plot_scores", "tower_coord", "make_tower_plot"))

pbapply::pblapply(X = plot_img_names, cl = cl, FUN = function(x) {
  make_tower_plot(x,
                  plot_scores,
                  tower_coord,
                  "output/2019-04-18/scored_philly/tower_plots")
})

parallel::stopCluster(cl)

# save slices to identify false positives
plot_scores <- scores[scores$predicted_probs >= 0.3 &
                        scores$predicted_probs < 0.4,]

bad_towers <- data.frame(
  img_id = stringr::str_match(plot_scores$source_img, "(.*/)*(.*)\\.(tif|png|jp2)")[,3],
  slice_id = plot_scores$slice_id,
  stringsAsFactors = FALSE
)

bad_towers <- bad_towers[order(bad_towers$img_id), ]

cl <- parallel::makeCluster(parallel::detectCores() - 1)

parallel::clusterEvalQ(cl, {
  library(magick)
})

pbapply::pblapply(X = split(bad_towers, bad_towers$img_id), cl = cl,
                  FUN = function(x) {
  img_id <- unique(x$img_id)

  img_df <- readRDS(file.path("output/2019-04-18/sliced_philly",
                              paste0(img_id, "_slices.rds")))

  img_df <- img_df[img_df$slice_id %in% x$slice_id,]

  for (i in seq_len(nrow(img_df))) {
    magick::image_write(
      image = image_read(drop(img_df[i, "slice_array"][[1]]) / 255),
      path = file.path("output/2019-04-18/slices_gte03_lt04",
                       paste0(img_id, "_", img_df[i, "slice_id"], ".png")))
  }

})

parallel::stopCluster(cl)

# save slices that intersect with tower coords + 25-ft radius
tower_coord_buff <- st_buffer(tower_coord$geometry, 25)

slices <- c(
  "philly-scores-lte001.rds",
  "philly-scores-gt001-lte0013.rds", "philly-scores-gt0013-lte0018.rds",
  "philly-scores-gt0018-lte0025.rds", "philly-scores-gt0025-lte0034.rds",
  "philly-scores-gt0034-lte0048.rds", "philly-scores-gt0048-lte007.rds",
  "philly-scores-gt007-lte0106.rds", "philly-scores-gt0106-lte0174.rds",
  "philly-scores-gt0174-lte0338.rds", "philly-scores-gt0338-lte1.rds"
  )

for (slice in slices) {

  scores <- readRDS(file.path("output/2019-04-18/scored_philly/", slice))

  slice_tower_intersect <- st_intersects(scores, tower_coord_buff)

  slices_intersect <- scores[
    sapply(slice_tower_intersect, function(x) length(x) > 0) == TRUE,
  ]

  slices_intersect <- data.frame(
    img_id = stringr::str_match(
      slices_intersect$source_img, "(.*/)*(.*)\\.(tif|png|jp2)")[,3],
    slice_id = slices_intersect$slice_id,
    stringsAsFactors = FALSE
  )

  cl <- parallel::makeCluster(length(unique(slices_intersect$img_id)))

  parallel::clusterEvalQ(cl, {
    library(magick)
  })

  pbapply::pblapply(X = split(slices_intersect, slices_intersect$img_id), cl = cl,
                    FUN = function(x) {
                      img_id <- unique(x$img_id)

                      img_df <- readRDS(file.path("output/2019-04-18/sliced_philly",
                                                  paste0(img_id, "_slices.rds")))

                      img_df <- img_df[img_df$slice_id %in% x$slice_id,]

                      for (i in seq_len(nrow(img_df))) {
                        magick::image_write(
                          image = image_read(drop(img_df[i, "slice_array"][[1]]) / 255),
                          path = file.path("output/2019-04-18/slices_tower-coord-intersect",
                                           paste0(img_id, "_", img_df[i, "slice_id"], ".png")))
                      }

                    })

  parallel::stopCluster(cl)
}

# examine towers with coords and those without
slices_nocoord <- list.dirs("output/2019-04-18", recursive = TRUE, full.names = TRUE)
slices_nocoord <- slices_nocoord[grepl("gte", slices_nocoord)]
slices_nocoord <- slices_nocoord[grepl("/tower", slices_nocoord)]
slices_nocoord <- list.files(slices_nocoord)

slices_coord <- list.files("output/2019-04-18/slices_tower-coord-intersect/tower")

slices_i_missed <- setdiff(slices_coord, slices_nocoord)
slices_they_missed <- setdiff(slices_nocoord, slices_coord)

# save all tower slices to curated training data
all_tower_slices <- unique(c(slices_coord, slices_nocoord))

all_tower_slices <- data.frame(
  img_id = stringr::str_match(all_tower_slices, "(.*6in)_(\\d*?)\\.png")[, 2],
  slice_id = stringr::str_match(all_tower_slices, "(.*6in)_(\\d*?)\\.png")[, 3],
  stringsAsFactors = FALSE
)

cl <- parallel::makeCluster(parallel::detectCores() - 1)

pbapply::pblapply(
  X = split(all_tower_slices, all_tower_slices$img_id), cl = cl,
            FUN = function(x) {
              img_id <- unique(x$img_id)

              img_df <- readRDS(file.path("output/2019-04-18/sliced_philly",
                                          paste0(img_id, "_slices.rds")))

              img_df <- img_df[img_df$slice_id %in% x$slice_id,]

              saveRDS(
                img_df,
                file.path("data/curated-training-slices/philly-towers",
                          paste0(img_id, "_tower_slices.rds"))
                )

            })

parallel::stopCluster(cl)

# save all high prob notower slices to curated training data
slices_notower <- list.dirs("output/2019-04-18", recursive = TRUE, full.names = TRUE)
slices_notower <- slices_notower[grepl("gte", slices_notower)]
slices_notower <- slices_notower[grepl("/notower", slices_notower)]
slices_notower <- list.files(slices_notower)

all_notower_slices <- data.frame(
  img_id = stringr::str_match(slices_notower, "(.*6in)_(\\d*?)\\.png")[, 2],
  slice_id = stringr::str_match(slices_notower, "(.*6in)_(\\d*?)\\.png")[, 3],
  stringsAsFactors = FALSE
)

cl <- parallel::makeCluster(parallel::detectCores() - 1)

pbapply::pblapply(
  X = split(all_notower_slices, all_notower_slices$img_id), cl = cl,
  FUN = function(x) {
    img_id <- unique(x$img_id)

    img_df <- readRDS(file.path("output/2019-04-18/sliced_philly",
                                paste0(img_id, "_slices.rds")))

    img_df <- img_df[img_df$slice_id %in% x$slice_id,]

    saveRDS(
      img_df,
      file.path("data/curated-training-slices/philly/philly-highprob-notowers",
                paste0(img_id, "_highprob-notower_slices.rds"))
    )

  })

parallel::stopCluster(cl)
