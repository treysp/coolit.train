calc_cell_neighbors <- function(num_cols) {
  tile_type <- rep("", num_cols ^ 2)
  tile_seq <- seq_len(num_cols ^ 2)

  # is it an edge tile?
  tile_type <- ifelse(tile_seq <= num_cols, "t", tile_type)
  tile_type <- ifelse(tile_seq %% num_cols == 0, "r", tile_type)
  tile_type <- ifelse(tile_seq > num_cols ^ 2 - num_cols, "b", tile_type)
  tile_type <- ifelse(tile_seq %% num_cols == 1, "l", tile_type)

  # is it a corner tile?
  tile_type <- ifelse(tile_seq == 1, "tl", tile_type)
  tile_type <- ifelse(tile_seq == num_cols, "tr", tile_type)
  tile_type <- ifelse(tile_seq == num_cols * num_cols, "br", tile_type)
  tile_type <- ifelse(tile_seq == num_cols ^ 2 - num_cols + 1, "bl", tile_type)

  # make neighbor list
  nb_list <- cell2nb(num_cols, num_cols, type = "queen")
  class(nb_list) <- "list"

  xy <- matrix(
    as.integer(
      unlist(
        strsplit(attr(nb_list, "region.id"), ":")
      )
    ),
    ncol=2, byrow=TRUE)

  names(nb_list) <- num_cols * (xy[,2] - 1) + xy[,1]

  # pad out neighbor vectors with NAs
  nb_list <- lapply(seq_along(nb_list), function(i) {
    tile_type <- tile_type[i]
    nb_vec <- nb_list[[i]]

    if (tile_type == "") return(nb_vec)

    if (tile_type == "t") {

      out <- c(rep(NA, 3),
               nb_vec)

    } else if (tile_type == "r") {

      out <- c(nb_vec[1:2], NA,
               nb_vec[3], NA,
               nb_vec[4:5], NA)

    } else if (tile_type == "b") {

      out <- c(nb_vec,
               rep(NA, 3))

    } else if (tile_type == "l") {

      out <- c(NA, nb_vec[1:2],
               NA, nb_vec[3],
               NA, nb_vec[4:5])

    } else if (tile_type == "tl") {

      out <- c(rep(NA, 3),
               NA, nb_vec[1],
               NA, nb_vec[2:3])

    } else if (tile_type == "tr") {

      out <- c(rep(NA, 3),
               nb_vec[1], NA,
               nb_vec[2:3], NA)

    } else if (tile_type == "br") {

      out <- c(nb_vec[1:2], NA,
               nb_vec[3], NA,
               rep(NA, 3))

    } else if (tile_type == "bl") {
      out <- c(NA, nb_vec[1:2],
               NA, nb_vec[3],
               rep(NA, 3))
    }

  out
  })

nb_list
}

nb_100 <- do.call("rbind", calc_neighbors(100))

scores_dt[, c("img_id", "tile_num") :=
            split(
              t(stringr::str_split_fixed(unique_id, "_", 2)),
              factor(c("img_num", "tile_num"))
              )
          ]

scores_dt[, tile_num := as.integer(tile_num)]

system.time({
  setkeyv(scores_dt, c("img_id", "tile_num"))
})

for (i in 1) {
  images <- scores_dt[tile_num == i, "img_id"]
  nb_index <- nb_100[i,]

  out <- lapply(seq_len(nrow(images)), function(j) {
    matrix(
      scores_dt[.(images[j], nb_index),
                predicted_probs],
      nrow = 1
    )
  })

}


temp <- data.table(img_id = c("a", "a", "a", "b", "b"), tile_num = 1:5, result = 1:5)
setkeyv(temp, c("img_id", "tile_num"))

temp["a"]

