#' Slice a jp2 image into tiles for scoring with a Keras model
#'
#' @param jp2_path Path to .jp2 image files
#' @param jp2_aux_path Path to .jp2.aux.xml file containing metadata about .jp2 image
#'
#' @param slice_n_rows Number of rows in each tile. See \code{\link{calc_slice_corners}}.
#' @param slice_n_cols Number of columns in each tile. See \code{\link{calc_slice_corners}}.
#'
#' @param slice_overlap Number of pixel overlap in adjacent tiles (in both X and Y directions).
#'                       See \code{\link{calc_slice_corners}}.
#'
#' @param complete_image If TRUE and the tile size and overlap dimensions do not conform to
#'                       covering the entire source raster/image, an additional row and column
#'                       of tiles will be created that include the excluded pixels but do NOT
#'                       respect the overlap value. If FALSE and the dimensions do not conform,
#'                       the set of tiles will omit some pixels on the right and bottom side
#'                       of the source raster/image. See \code{\link{calc_slice_corners}}.
#'
#' @param verbose Should messages about current step being processes be printed to screen?
#'
#' @return Data frame with one row for each tile, containing:
#' - \code{jp2_path}
#' - \code{jp2_aux_path}
#' - Numeric tile ID number
#' - Tile corner cells in source jp2 pixel values
#' - List-column containing each tile's extent based on the source jp2's projected raster
#' - sf geometry column containing each tile's sf polygon
#' - List-column containing tile's RGB layers in a 4d array of dimension
#' \[1, \code{slice_n_cols}, \code{slice_n_rows}, 3\]
#'
#' @export
#' @importFrom xml2 as_list read_xml
#' @importFrom sf st_crs st_sfc st_polygon
#' @importFrom raster brick nlayers dropLayer extent crs
#' @importFrom pbapply pblapply pboptions
#' @importFrom abind abind
slice_jp2_image <- function(jp2_path, jp2_aux_path,
                            slice_n_rows, slice_n_cols,
                            slice_overlap = 0, complete_image = FALSE,
                            verbose = FALSE) {
  if (!verbose) {
    opb <- pbapply::pboptions(type="none")
    on.exit(pboptions(opb))
  }

  # error checking
  if (!grepl("\\.jp2$", jp2_path)) {
    stop("`jp2_path` must point to a file ending in '.jp2'")
  }

  # read jp2 aux for projection info
  if (verbose) message("Reading image.")
  jp2_xml <- xml2::as_list(xml2::read_xml(jp2_aux_path))

  source_brick <- raster::brick(jp2_path)

  crs(source_brick) <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string

  if (nlayers(source_brick) > 3) {
    source_brick <- raster::dropLayer(source_brick, 4)
  }

  # create raster extents and sf polygons for each tile
  tile_data <- calc_slice_corners(
    source_n_rows = nrow(source_brick),
    source_n_cols = ncol(source_brick),
    slice_n_rows = slice_n_rows,
    slice_n_cols = slice_n_cols
  )

  tile_data <- cbind(source_img = jp2_path,
                     source_img_aux = jp2_aux_path,
                     tile_id = seq_len(nrow(tile_data)),
                     tile_data,
                     stringsAsFactors = FALSE)

  tile_extents <- split(tile_data, 1:NROW(tile_data))

  if (verbose) message("Creating extents:")
  tile_data$extents <- pblapply(tile_extents, function(x) {
    extent(source_brick, x[["y0"]], x[["y1"]], x[["x0"]], x[["x1"]])
  })

  if (verbose) message("Creating polygons:")
  tile_data$geometry <- pblapply(tile_data$extents, function(x) {
    st_sfc(
      st_polygon(
        list(
          rbind(
            c(x@xmin, x@ymin),
            c(x@xmin, x@ymax),
            c(x@xmax, x@ymax),
            c(x@xmax, x@ymin),
            c(x@xmin, x@ymin)
          )
        )
      ),
      crs = crs(source_brick)@projargs
    )
  })

  # create a 4d array for each tile, containing 1 [50, 50, 3] slice
  if (verbose) message("Converting image to array and slicing - may take a minute.")
  source_brick_data <- raster::as.array(source_brick)

  tile_data$tile_array <- lapply(1:nrow(tile_data), function(i) {
    out <- array(NA, c(1, slice_n_cols, slice_n_rows, 3))

    out[1, , ,] <- source_brick_data[
      seq(tile_data[["y0"]][i], tile_data[["y1"]][i]),
      seq(tile_data[["x0"]][i], tile_data[["x1"]][i])
      ,
      ]

    out
  })

  # # must transpose each layer to reassemble original brick
  # temp <- crop(source_brick, tile_data$extents[[1]])
  # plotRGB(temp)
  #
  # temp_array <- drop(tile_data$tile_array[[1]])
  # temp_array[,,1] <- t(temp_array[,,1])
  # temp_array[,,2] <- t(temp_array[,,2])
  # temp_array[,,3] <- t(temp_array[,,3])
  # plotRGB(temp)

  tile_data
}