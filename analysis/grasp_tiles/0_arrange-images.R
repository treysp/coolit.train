source_dirs <- list.dirs("data/source", recursive = FALSE)

lapply(source_dirs[-c(1, 5)], function(x) {
  cat(x, "\n")
  all_images <- list.files(x, full.names = TRUE)
  all_images <- all_images[grepl("\\.jpg", all_images)]
  
  pos_images <- all_images[grepl("_yes", all_images)]
  neg_images <- all_images[!grepl("_yes", all_images)]
  
  dir.create(file.path(x, "tower"))
  file.copy(pos_images, file.path(x, "tower"))
  
  dir.create(file.path(x, "notower"))
  file.copy(neg_images, file.path(x, "notower"))
  
})

# identify all pos and neg images, move to data structure
#   expected by flow_images_from_directory()
all_dirs <- list.dirs("data/source")
all_dirs <- all_dirs[grepl("tower", all_dirs)]
neg_dirs <- all_dirs[grepl("notower", all_dirs)]
pos_dirs <- setdiff(all_dirs, neg_dirs)

pos_images <- unlist(lapply(pos_dirs, list.files, full.names = TRUE))
neg_images <- unlist(lapply(neg_dirs, list.files, full.names = TRUE))

dir.create("data/flow")
dir.create("data/flow/train")
dir.create("data/flow/train/tower")
dir.create("data/flow/train/notower")
dir.create("data/flow/validation")
dir.create("data/flow/validation/tower")
dir.create("data/flow/validation/notower")

set.seed(794)

pos_order <- sample.int(length(pos_images))
pos_images <- pos_images[pos_order] # randomly order images
pos_train_index <- seq(1, floor(length(pos_images) * .5)) # half for training
pos_valid_index <- max(pos_train_index) + seq(1, floor(length(pos_images) * .25)) # 1/4 for valid
file.copy(pos_images[pos_train_index], "data/flow/train/tower")
file.copy(pos_images[pos_valid_index], "data/flow/validation/tower")

neg_order <- sample.int(length(neg_images))
neg_images <- neg_images[neg_order]
neg_train_index <- seq(1, floor(length(neg_images) * .5))
neg_valid_index <- max(neg_train_index) + seq(1, floor(length(neg_images) * .25))
file.copy(neg_images[neg_train_index], "data/flow/train/notower")
file.copy(neg_images[neg_valid_index], "data/flow/validation/notower")

