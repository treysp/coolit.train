library(coolit)
library(stringr)
library(raster)
library(sf)
library(magick)

set.seed(1)

# PARAMETER: total number randomly selected notower images to use
num_notower <- 10000

# where images should live
base_dir <- "data/model-training-data/slices_curated_2019-05-16"

dir.create(base_dir)

for (i in c("train", "validation", "test")) {

  dir.create(file.path(base_dir, i))
  dir.create(file.path(base_dir, i, "tower"))
  dir.create(file.path(base_dir, i, "notower"))

}

#### NYC preparation --------------------------------------------------------------------
# get obstructed tower slice list
nyc_obstructed <- readr::read_csv(
  file.path("data/curated-training-slices/nyc/nyc-tower-slices",
            "nyc_obstructed-tower-slice-list_2019-04-23.csv"))

nyc_obstructed$exclude <- TRUE

# get tower slices
nyc_tower_slices <- lapply(
  list.files("data/curated-training-slices/nyc/nyc-tower-slices",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

nyc_tower_slices <- do.call("rbind", nyc_tower_slices)

nyc_tower_slices$img_num <- str_match(nyc_tower_slices$source_img,
                                      "(.*/)(\\d*)\\.jp2")[, 3]

# remove obstructed towers
nyc_tower_slices <- merge(nyc_tower_slices,
                          nyc_obstructed[, c("img_num", "tile_id", "exclude")],
                          by = c("img_num", "tile_id"),
                          all.x = TRUE)

nyc_tower_slices <- nyc_tower_slices[is.na(nyc_tower_slices$exclude),]
nyc_tower_slices$exclude <- NULL

# assign train/validation/test status
nyc_tower_slices$status <- sample(c("train", "validation", "test"),
                                  size = nrow(nyc_tower_slices),
                                  prob = c(.7, .2, .1),
                                  replace = TRUE)

nyc_tower_slices$out_name <- paste0(base_dir, "/",
                                    nyc_tower_slices$status, "/",
                                    "tower/",
                                    nyc_tower_slices$img_num, "_",
                                    nyc_tower_slices$tile_id, ".png")

nyc_tower_index <- nyc_tower_slices[, c("img_num", "tile_id")]
nyc_tower_index$exclude <- TRUE

# count tower/img for notower sampling
nyc_tower_img_count <- table(nyc_tower_slices$img_num)
nyc_tower_img_count <- data.frame(
  img_num = names(nyc_tower_img_count),
  count = as.integer(nyc_tower_img_count),
  stringsAsFactors = FALSE
)

nyc_tower_img_count$prop <- nyc_tower_img_count$count /
  sum(nyc_tower_img_count$count)

# get high-prob notower slices
nyc_highprob <- lapply(
  list.files("data/curated-training-slices/nyc/nyc-highprob-notowers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

nyc_highprob <- do.call("rbind", nyc_highprob)

nyc_highprob$img_num <- str_match(nyc_highprob$source_img,
                                  "(.*/)(\\d*)\\.jp2")[, 3]

nyc_highprob$status <- sample(c("train", "validation", "test"),
                              size = nrow(nyc_highprob),
                              prob = c(.7, .2, .1),
                              replace = TRUE)

nyc_highprob$out_name <- paste0(base_dir, "/",
                                nyc_highprob$status, "/",
                                "notower/",
                                nyc_highprob$img_num, "_",
                                nyc_highprob$tile_id, ".png")

#### Philly preparation --------------------------------------------------------------------
# get tower slices
philly_tower_slices <- lapply(
  list.files("data/curated-training-slices/philly/philly-towers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

philly_tower_slices <- do.call("rbind", philly_tower_slices)

philly_tower_slices$img_num <- str_match(philly_tower_slices$source_img,
                                         "(.*/)(.*)\\.tif")[, 3]

# assign train/validation/test status
philly_tower_slices$status <- sample(c("train", "validation", "test"),
                                     size = nrow(philly_tower_slices),
                                     prob = c(.7, .2, .1),
                                     replace = TRUE)

philly_tower_slices$out_name <- paste0(base_dir, "/",
                                       philly_tower_slices$status, "/",
                                       "tower/",
                                       philly_tower_slices$img_num, "_",
                                       philly_tower_slices$slice_id, ".png")

philly_tower_index <- philly_tower_slices[, c("img_num", "slice_id")]
philly_tower_index$exclude <- TRUE

# count tower/img for notower sampling
philly_tower_img_count <- table(philly_tower_slices$img_num)
philly_tower_img_count <- data.frame(
  img_num = names(philly_tower_img_count),
  count = as.integer(philly_tower_img_count),
  stringsAsFactors = FALSE
)

philly_tower_img_count$prop <- philly_tower_img_count$count /
  sum(philly_tower_img_count$count)

# get high-prob notower slices
philly_highprob <- lapply(
  list.files("data/curated-training-slices/philly/philly-highprob-notowers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

philly_highprob <- do.call("rbind", philly_highprob)

philly_highprob$img_num <- str_match(philly_highprob$source_img,
                                  "(.*/)(.*)\\.tif")[, 3]

philly_highprob$status <- sample(c("train", "validation", "test"),
                                 size = nrow(philly_highprob),
                                 prob = c(.7, .2, .1),
                                 replace = TRUE)

philly_highprob$out_name <- paste0(base_dir, "/",
                                   philly_highprob$status, "/",
                                   "notower/",
                                   philly_highprob$img_num, "_",
                                   philly_highprob$slice_id, ".png")

#### Chi preparation --------------------------------------------------------------------
# get tower slices
chi_tower_slices <- lapply(
  list.files("data/curated-training-slices/chi/chi-towers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

chi_tower_slices <- do.call("rbind", chi_tower_slices)

chi_tower_slices$img_num <- chi_tower_slices$img_id

# assign train/validation/test status
chi_tower_slices$status <- sample(c("train", "validation", "test"),
                                     size = nrow(chi_tower_slices),
                                     prob = c(.7, .2, .1),
                                     replace = TRUE)

chi_tower_slices$out_name <- paste0(base_dir, "/",
                                       chi_tower_slices$status, "/",
                                       "tower/",
                                       chi_tower_slices$img_num, "_",
                                       chi_tower_slices$slice_id, ".png")

chi_tower_index <- chi_tower_slices[, c("img_num", "slice_id")]
chi_tower_index$exclude <- TRUE

# count tower/img for notower sampling
chi_tower_img_count <- table(chi_tower_slices$img_num)
chi_tower_img_count <- data.frame(
  img_num = names(chi_tower_img_count),
  count = as.integer(chi_tower_img_count),
  stringsAsFactors = FALSE
)

chi_tower_img_count$prop <- chi_tower_img_count$count /
  sum(chi_tower_img_count$count)

# get high-prob notower slices
chi_highprob <- lapply(
  list.files("data/curated-training-slices/chi/chi-highprob-notower",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

chi_highprob <- do.call("rbind", chi_highprob)

chi_highprob$img_num <- chi_highprob$img_id

chi_highprob$status <- sample(c("train", "validation", "test"),
                                 size = nrow(chi_highprob),
                                 prob = c(.7, .2, .1),
                                 replace = TRUE)

chi_highprob$out_name <- paste0(base_dir, "/",
                                   chi_highprob$status, "/",
                                   "notower/",
                                   chi_highprob$img_num, "_",
                                   chi_highprob$slice_id, ".png")

#### write images ---------------------------------------------------------------------
# setup cluster
ncores <- parallel::detectCores() - 1

cl <- parallel::makeCluster(ncores, outfile = "c:/users/wfu3/desktop/log.txt")

parallel::clusterEvalQ(cl, {
  library(stringr)
  library(sf)
  library(magick)
})

parallel::clusterExport(cl, c("base_dir",
                              "nyc_tower_index", "nyc_tower_img_count",
                              "philly_tower_index", "philly_tower_img_count",
                              "chi_tower_index", "chi_tower_img_count"))

out <- list()

# tower images
## nyc
nyc_tower_list <- split(nyc_tower_slices,
                        make_split_index(nrow(nyc_tower_slices), ncores))

out[[1]] <- pbapply::pblapply(X = nyc_tower_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

## philly
philly_tower_list <- split(philly_tower_slices,
                           make_split_index(nrow(philly_tower_slices), ncores))

out[[2]] <- pbapply::pblapply(X = philly_tower_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    temp <- drop(x[i, "slice_array", drop = TRUE][[1]])
    temp[is.na(temp)] <- 255

    magick::image_write(
      image = image_read(temp / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

## chi
chi_tower_list <- split(chi_tower_slices,
                        make_split_index(nrow(chi_tower_slices), ncores))

out[[1]] <- pbapply::pblapply(X = chi_tower_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "slice_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

# highprob notower images
## nyc
nyc_highprob_list <- split(nyc_highprob,
                           make_split_index(nrow(nyc_highprob), ncores))

out[[3]] <- pbapply::pblapply(X = nyc_highprob_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

## philly
philly_highprob_list <- split(philly_highprob,
                              make_split_index(nrow(philly_highprob), ncores))

out[[4]] <- pbapply::pblapply(X = philly_highprob_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    temp <- drop(x[i, "slice_array", drop = TRUE][[1]])
    temp[is.na(temp)] <- 255

    magick::image_write(
      image = image_read(temp / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

## chi
chi_highprob_list <- split(chi_highprob,
                           make_split_index(nrow(chi_highprob), ncores))

out[[4]] <- pbapply::pblapply(X = chi_highprob_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "slice_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

# sample and write notower images
tower_props <- c(nyc = nrow(nyc_tower_slices),
                 philly = nrow(philly_tower_slices),
                 chi = nrow(chi_tower_slices))

tower_props <- tower_props / sum(tower_props)

## nyc
nyc_tower_img_count$num_to_write <- ceiling(
  nyc_tower_img_count$prop * num_notower * tower_props[["nyc"]]
)

nyc_tower_img_count_list <- split(
  nyc_tower_img_count,
  nyc_tower_img_count$img_num
  )

out[[5]] <- parallel::parLapplyLB(
  X = nyc_tower_img_count_list,
  cl = cl,
  FUN = function(rds) {

  working_name <- file.path("data/curated-training-slices/nyc/nyc-slices",
                            paste0(unique(rds$img_num), "_slices.rds"))

  working <- readRDS(working_name)

  working$img_num <- str_match(working$source_img,
                               "(.*/)(\\d*)\\.jp2")[, 3]

  working <- merge(working,
                   nyc_tower_index,
                   by = c("img_num", "tile_id"),
                   all.x = TRUE)
  working <- working[is.na(working$exclude), ]

  num_to_write <- rds$num_to_write

  working <- working[sample(seq_len(nrow(working)), num_to_write), ]

  working$status <- sample(c("train", "validation", "test"),
                                   size = nrow(working),
                                   prob = c(.7, .2, .1),
                                   replace = TRUE)

  working$out_name <- paste0(base_dir, "/",
                             working$status, "/",
                             "notower/",
                             working$img_num, "_",
                             working$tile_id, ".png")

  for (i in seq_len(nrow(working))) {
    magick::image_write(
      image = image_read(drop(working[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = working[i, "out_name", drop = TRUE])
  }
})

## philly
philly_tower_img_count$num_to_write <- ceiling(
  philly_tower_img_count$prop * num_notower * tower_props[["philly"]]
)

philly_tower_img_count_list <- split(
  philly_tower_img_count,
  philly_tower_img_count$img_num
)

out[[6]] <- parallel::parLapplyLB(
  X = philly_tower_img_count_list,
  cl = cl,
  FUN = function(rds) {

  working_name <- file.path("data/curated-training-slices/philly/philly-slices",
                            paste0(unique(rds$img_num), "_slices.rds"))

  working <- readRDS(working_name)

  working$img_num <- str_match(working$source_img,
                               "(.*/)(.*)\\.tif")[, 3]

  working <- merge(working,
                   philly_tower_index,
                   by = c("img_num", "slice_id"),
                   all.x = TRUE)
  working <- working[is.na(working$exclude), ]

  num_to_write <- rds$num_to_write

  working <- working[sample(seq_len(nrow(working)), num_to_write), ]

  working$status <- sample(c("train", "validation", "test"),
                           size = nrow(working),
                           prob = c(.7, .2, .1),
                           replace = TRUE)

  working$out_name <- paste0(base_dir, "/",
                             working$status, "/",
                             "notower/",
                             working$img_num, "_",
                             working$tile_id, ".png")

  for (i in seq_len(nrow(working))) {
    temp <- drop(working[i, "slice_array", drop = TRUE][[1]])
    temp[is.na(temp)] <- 255

    magick::image_write(
      image = image_read(temp / 255),
      path = working[i, "out_name", drop = TRUE])
  }
})

## chi
chi_tower_img_count$num_to_write <- ceiling(
  chi_tower_img_count$prop * num_notower * tower_props[["chi"]]
)

chi_tower_img_count_list <- split(
  chi_tower_img_count,
  chi_tower_img_count$img_num
)

chi_tower_index$geometry <- NULL


out[[5]] <- parallel::parLapplyLB(
  X = chi_tower_img_count_list,
  cl = cl,
  fun = function(rds) {

    working_name <- file.path("data/curated-training-slices/chi/chi-slices",
                              paste0(unique(rds$img_num), "_slices.rds"))

    working <- readRDS(working_name)

    working$img_num <- unique(rds$img_num)

    working <- merge(working,
                     chi_tower_index,
                     by = c("img_num", "slice_id"),
                     all.x = TRUE)
    working <- working[is.na(working$exclude), ]

    num_to_write <- rds$num_to_write

    working <- working[sample(seq_len(nrow(working)), num_to_write), ]

    working$status <- sample(c("train", "validation", "test"),
                             size = nrow(working),
                             prob = c(.7, .2, .1),
                             replace = TRUE)

    working$out_name <- paste0(base_dir, "/",
                               working$status, "/",
                               "notower/",
                               working$img_num, "_",
                               working$tile_id, ".png")

    for (i in seq_len(nrow(working))) {
      magick::image_write(
        image = image_read(drop(working[i, "slice_array", drop = TRUE][[1]]) / 255),
        path = working[i, "out_name", drop = TRUE])
    }
  })

parallel::stopCluster(cl)
