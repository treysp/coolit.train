#' Calculate the tile corner XY values in source raster/image grid
#'
#' @param source_n_rows Number of rows in source raster/image
#' @param source_n_cols Number of columns in source raster/image
#'
#' @param slice_n_rows Number of rows in each tile
#' @param slice_n_cols Number of columns in each tile
#'
#' @param slice_overlap Number of pixel overlap in adjacent tiles (in both X and Y directions)
#'
#' @param complete_image If TRUE and the tile size and overlap dimensions do not conform to
#'                       covering the entire source raster/image, an additional row and column
#'                       of tiles will be created that include the excluded pixels but do NOT
#'                       respect the overlap value. If FALSE and the dimensions do not conform,
#'                       the set of tiles will omit some pixels on the right and bottom side
#'                       of the source raster/image
#'
#' @return Data frame with corner cells of one tile in each row, containing
#' variables \['x0', 'x1', 'y0', 'y1'\] for start \(\*0\\) and stop \(\*1)
#' cell numbers in horizontal \(x\*\) and vertical \(y\*\) directions
#'
#' @export
#' @importFrom tidyr expand nesting
calc_slice_corners <- function(source_n_rows, source_n_cols,
                               slice_n_rows, slice_n_cols,
                               slice_overlap = 0, complete_image = FALSE) {
  slice_row_over <- slice_n_rows - slice_overlap
  slice_col_over <- slice_n_cols - slice_overlap

  r1 <- seq(1, slice_row_over * (source_n_rows %/% slice_row_over), by = slice_row_over)
  r1 <- r1[r1 <= (source_n_rows - slice_n_rows + 1)]
  r2 <- r1 + (slice_n_rows - 1)

  if (complete_image && max(r2) < source_n_rows) {
    r1 <- c(r1, source_n_rows - slice_n_rows + 1)
    r2 <- c(r2, source_n_rows)
  }

  c1 <- seq(1, slice_col_over * (source_n_cols %/% slice_col_over), by = slice_col_over)
  c1 <- c1[c1 <= (source_n_cols - slice_n_cols + 1)]
  c2 <- c1 + (slice_n_cols - 1)

  if (complete_image && max(c2) < source_n_rows) {
    c1 <- c(c1, source_n_cols - slice_n_cols + 1)
    c2 <- c(c2, source_n_cols)
  }

  corners_df <- data.frame(x0 = c1, x1 = c2, y0 = r1, y1 = r2)
  corners_df <- tidyr::expand(
    corners_df,
    tidyr::nesting(x0, x1),
    tidyr::nesting(y0, y1)
  )

  corners_df <- corners_df[order(corners_df$y0, corners_df$x0),]

  if (is.na(NROW(corners_df)) || NROW(corners_df) == 0) {
    stop("Error defining tile corners...please investigate.")
  }

  corners_df
}