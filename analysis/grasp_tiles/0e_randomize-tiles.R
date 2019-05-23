set.seed(687436)

train_frac <- .7
test_frac <- .15
validate_frac <- .15

orig_tower_dir <- "C:/Users/wfu3/Desktop/temp/tiles/orig/tiles_tower"
orig_notower_dir <- "C:/Users/wfu3/Desktop/temp/tiles/orig/tiles_notower"

train_dir <- "C:/Users/wfu3/Desktop/temp/tiles/orig/train"
test_dir <- "C:/Users/wfu3/Desktop/temp/tiles/orig/test"
validate_dir <- "C:/Users/wfu3/Desktop/temp/tiles/orig/validate"

split_data_dir <- "C:/Users/wfu3/Desktop/temp/tiles/orig"

# create random partitions
splits <- list(tower = list(), notower = list())

## tower files
orig_tower_files <- list.files(orig_tower_dir, full.names = TRUE)
shuffle_tower_files <- orig_tower_files[sample.int(length(orig_tower_files))]
all(sort(orig_tower_files) == sort(shuffle_tower_files))

index_tower_train <- floor(train_frac * length(orig_tower_files))
index_tower_test <- index_tower_train + floor(test_frac * length(orig_tower_files))
index_tower_valid <- length(orig_tower_files)

splits$tower$train <- shuffle_tower_files[1:index_tower_train]
splits$tower$test <- shuffle_tower_files[(index_tower_train + 1):index_tower_test]
splits$tower$valid <- shuffle_tower_files[(index_tower_test + 1):index_tower_valid]
all(sort(unlist(splits$tower)) == sort(shuffle_tower_files))

new_loc <- function(files, old_dir, new_dir, append) {
  files <- stringr::str_replace(files, old_dir, "")
  fs::path(new_dir, append, files)
}

fs::file_copy(splits$tower$train, 
              new_loc(splits$tower$train, orig_tower_dir, train_dir, "tower"))

fs::file_copy(splits$tower$test, 
              new_loc(splits$tower$test, orig_tower_dir, test_dir, "tower"))

fs::file_copy(splits$tower$valid, 
              new_loc(splits$tower$valid, orig_tower_dir, validate_dir, "tower"))

## no tower files
orig_notower_files <- list.files(orig_notower_dir, full.names = TRUE)
shuffle_notower_files <- orig_notower_files[sample.int(length(orig_notower_files))]
all(sort(orig_notower_files) == sort(shuffle_notower_files))

index_notower_train <- floor(train_frac * length(orig_notower_files))
index_notower_test <- index_notower_train + floor(test_frac * length(orig_notower_files))
index_notower_valid <- length(orig_notower_files)

splits$notower$train <- shuffle_notower_files[1:index_notower_train]
splits$notower$test <- shuffle_notower_files[(index_notower_train + 1):index_notower_test]
splits$notower$valid <- shuffle_notower_files[(index_notower_test + 1):index_notower_valid]
all(sort(unlist(splits$notower)) == sort(shuffle_notower_files))

fs::file_copy(splits$notower$train, 
              new_loc(splits$notower$train, orig_notower_dir, train_dir, "notower"))

fs::file_copy(splits$notower$test, 
              new_loc(splits$notower$test, orig_notower_dir, test_dir, "notower"))

fs::file_copy(splits$notower$valid, 
              new_loc(splits$notower$valid, orig_notower_dir, validate_dir, "notower"))

## record splits for future use
splits_df <- purrr::modify_depth(splits, 2, 
                                 function(x) data.frame(tile = x, stringsAsFactors = FALSE))
splits_df <- dplyr::bind_rows(
  lapply(splits_df, function(x) dplyr::bind_rows(x, .id = "split")), 
  .id = "tower"
  )

write.csv(splits_df, 
          row.names = FALSE,
          file = file.path(split_data_dir, paste0("splits_", Sys.Date(), ".csv")))
