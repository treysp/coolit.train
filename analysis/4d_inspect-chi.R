library(raster)
library(sf)
library(data.table)
library(magick)
library(dplyr)

scores <- readRDS("output/2019-05-07/chi-scores/chi-scores-gt0417-lte1.rds")
class(scores) <- c("data.frame", "sf")

scores_gt995 <- scores[scores$predicted_probs > .98 & scores$predicted_probs <= .985, ]

scores_gt995$img_num <- stringr::str_match(scores_gt995$img_name,
                                           "(.*/)(\\d{1,4})_.*$")[, 3]
scores_gt995$img_stub <- paste0(scores_gt995$img_num, "-",
                                scores_gt995$slice_id, ".png")

scores_list <- split(scores_gt995[, c("img_num", "slice_id", "img_stub")],
                     scores_gt995$img_num)

cl <- parallel::makeCluster(parallel::detectCores() - 1)

parallel::clusterEvalQ(cl, {
  library(raster)
  library(sf)
  library(magick)
})

parallel::parLapplyLB(X = scores_list, cl = cl, fun = function(to_score) {
  img_slices <- readRDS(
    paste0("data/source_from-chi-website/chi-slices/",
           unique(to_score$img_num),
           "_slices.rds")
    )

  img_slices <- img_slices[img_slices$slice_id %in% to_score$slice_id,]

  for (i in 1:nrow(img_slices)) {
    image_write(
      image_read(drop(img_slices$slice_array[[i]]) / 255),
      paste0("c:/users/wfu3/desktop/temp/", to_score$img_stub[[i]])
    )
  }
})

parallel::stopCluster(cl)

# compare model scores to towers hand-identified from GRASP coordinates
model_towers <- c(
  fs::dir_ls("output/2019-05-07/chi-scores/gt995/tower", type = "file"),
  fs::dir_ls("output/2019-05-07/chi-scores/gt990_lte995/tower", type = "file"),
  fs::dir_ls("output/2019-05-07/chi-scores/gt985_lte99/tower", type = "file"),
  fs::dir_ls("output/2019-05-07/chi-scores/gt98_lte985/tower", type = "file")
)

model_towers <- str_match(model_towers, "(.*/)*(\\d{1,4})-(\\d{1,4})\\.png$")[, 3:4]

model_towers <- data.frame(
  img_id = as.numeric(model_towers[, 1]),
  slice_id = as.numeric(model_towers[, 2]),
  model_tower = 1
)

hand_towers <- fs::dir_ls("data/curated-training-slices/chi/chi-tower-images", type = "file")

hand_towers <- str_match(hand_towers, "(.*/)*(\\d{1,4})_(\\d{1,4})\\.png$")[, 3:4]

hand_towers <- data.frame(
  img_id = as.numeric(hand_towers[, 1]),
  slice_id = as.numeric(hand_towers[, 2]),
  hand_tower = 1
)

hand_towers <- anti_join(hand_towers, model_towers)

all_towers <- bind_rows(model_towers, hand_towers)

my_scores <- readRDS("output/2019-05-07/chi-scores/chi-scores-gt0417-lte1.rds")
my_scores$img_id <- as.numeric(
  str_match(my_scores$img_name,
            "(.*/)*(\\d{1,4})_slices_scores\\.rds$")[, 3]
  )

hand_towers <- left_join(hand_towers,
                         my_scores[, c("img_id", "slice_id", "predicted_probs")],
                         by = c("img_id", "slice_id"))

hand_slices <- fs::dir_ls("c:/users/wfu3/desktop/temp/notower",
                          type = "file", recursive = TRUE)
hand_slices <- str_match(hand_slices, "(.*/)*(\\d{1,4})_(\\d{1,4})\\.png")[, 3:4]
hand_slices <- data.frame(
  img_id = as.numeric(hand_slices[,1]),
  slice_id = as.numeric(hand_slices[,2]),
  hand_slice = 1
)

model_only_towers <- left_join(model_only_towers, hand_slices,
                               by = c("img_id", "slice_id"))

# make highprob-notower slices
my_scores <- readRDS("output/2019-05-07/chi-scores/chi-scores-gt0417-lte1.rds")
my_scores$img_id <- as.numeric(
  str_match(my_scores$img_name,
            "(.*/)*(\\d{1,4})_slices_scores\\.rds$")[, 3]
)

all_towers <- fs::dir_ls("data/curated-training-slices/chi/chi-tower-images", type = "file")

all_towers <- str_match(all_towers, "(.*/)*(\\d{1,4})_(\\d{1,4})\\.png$")[, 3:4]

all_towers <- data.frame(
  img_id = as.numeric(all_towers[, 1]),
  slice_id = as.numeric(all_towers[, 2])
)

my_scores_90 <- my_scores[my_scores$predicted_probs > .9,]

my_scores_90 <- anti_join(my_scores_90, all_towers, by = c("img_id", "slice_id"))
my_scores_90$geometry <- NULL
my_scores_90 <- my_scores_90[, c("img_id", "slice_id")]
my_scores_90$highprob_notower <- TRUE

all_image_id <- unique(c(all_towers$img_id, my_scores_90$img_id))
temp <- list.files("data/curated-training-slices/chi/chi-highprob-notower")
temp <- str_match(temp, "(\\d{1,4}).*")[, 2]
temp <- as.numeric(temp)
all_image_id <- all_image_id[!(all_image_id %in% temp)]

cl <- parallel::makeCluster(15)

parallel::clusterEvalQ(cl, {
  library(dplyr)
  library(raster)
  library(sf)
})
parallel::clusterExport(cl, c("all_towers", "my_scores_90"))

pbapply::pblapply(X = all_image_id, cl = cl, FUN = function(img) {
  temp_slices <- readRDS(paste0(
    "data/curated-training-slices/chi/chi-slices/",
    img,
    "_slices.rds")
  )

  temp_slices$img_id <- img

  temp_tower <- inner_join(temp_slices, all_towers, by = c("img_id", "slice_id"))

  if (nrow(temp_tower) > 0) {
    saveRDS(
      temp_tower,
      paste0(
        "data/curated-training-slices/chi/chi-towers/",
        img,
        "_tower_slices.rds"
      )
    )
  }

  temp_notower <- inner_join(temp_slices, my_scores_90,
                             by = c("img_id", "slice_id"))

  if (nrow(temp_notower) > 0) {
    saveRDS(
      temp_notower,
      paste0(
        "data/curated-training-slices/chi/chi-highprob-notower/",
        img,
        "_highprob-notower_slices.rds"
      )
      )
  }
})

parallel::stopCluster(cl)
