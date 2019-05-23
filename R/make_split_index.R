#' create integer sequence for splitting vectors into a list
#'
#' @param vec_length length of the output vector
#' @param num_chunks number of unique values in the output vector
#'
#' @export
make_split_index <- function(vec_length, num_chunks) {
  first_each <- vec_length %/% num_chunks

  num_extra_each <- vec_length - (first_each * num_chunks)

  index <- rep(1:(num_chunks - num_extra_each), each = first_each)

  index <- c(
    index,
    rep(seq(tail(index, 1) + 1, num_chunks), each = first_each + 1)
  )

  index
}
