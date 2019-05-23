#' Slice an image into tiles for scoring with a Keras model
#'
#' @param img_object Image object that can be read by \code{\link[raster]{brick}}.
#' @param img_path Path to image file that can be read by \code{\link[raster]{brick}}.
#'                 Overridden by `img_object`.
#'
#' @param slice_n_rows Number of rows in each tile.
#'                     See \code{\link{calc_slice_corners}}. Required.
#' @param slice_n_cols Number of columns in each tile.
#'                     See \code{\link{calc_slice_corners}}. Required.
#'
#' @param slice_overlap_px Number of pixel overlap in adjacent tiles (in both X and Y directions).
#'                         See \code{\link{calc_slice_corners}}.
#'
#' @param complete_image If TRUE and the tile size and overlap dimensions do not conform to
#'                       covering the entire source raster/image, an additional row and column
#'                       of tiles will be created that include the excluded pixels but do NOT
#'                       respect the overlap value. If FALSE and the dimensions do not conform,
#'                       the set of tiles will omit some pixels on the right and bottom side
#'                       of the source raster/image. See \code{\link{calc_slice_corners}}.
#'
#' @param img_xml_wkt_path Optional path to .xml file containing metadata about
#'                         image in WKT format.
#' @param wkt_string Optional WKT string containing image projection information.
#'                   Overridden by `img_xml_wkt_path` if present.
#' @param proj4_string Optional proj4 string containing image projection information.
#'                     Overridden by `img_xml_wkt_path` of `wkt_string` if present.
#'
#' @param reproject_raster Should raster be re-projected to projection specified in
#'                         `img_xml_wkt_path`, `wkt_string`, or `proj4_string`?
#'
#' @param return_sf Should output data frame be an sf object (default regular data frame)?
#'
#' @param verbose Should messages about current step being processes be printed to screen?
#'
#' @return Data frame with one row for each tile, containing:
#' - \code{img_path}
#' - \code{img_xml_wkt_path} (if present)
#' - Numeric tile ID number
#' - Tile corner cells in source pixel values
#' - List-column containing each tile's extent based on the source jp2's projected raster
#' - sf geometry column containing each tile's sf polygon
#' - List-column containing tile's RGB layers in a 4d array of dimension
#' [1, \code{slice_n_cols}, \code{slice_n_rows}, 3]
#'
#' @export
#' @importFrom xml2 as_list read_xml
#' @importFrom sf st_crs st_sfc st_polygon
#' @importFrom raster brick nlayers dropLayer extent crs
#' @importFrom pbapply pblapply pboptions
#' @importFrom abind abind
slice_image <- function(img_object, img_path,
                        slice_n_rows, slice_n_cols,
                        slice_overlap_px = 0, complete_image = FALSE,
                        img_xml_wkt_path = NULL,
                        wkt_string = NULL, proj4_string = NULL,
                        reproject_raster = FALSE, return_sf = FALSE,
                        verbose = FALSE) {
  if (!verbose) {
    opb <- pbapply::pboptions(type="none")
    on.exit(pboptions(opb))
  }

  if (missing(img_object) & missing(img_path)) {
    stop("One of `img_object` or `img_path` is required.")
  }

  if (missing(slice_n_rows) || missing(slice_n_cols)) {
    stop("Arguments `slice_n_rows` and `slice_n_cols` are required.")
  }

  # read image
  if (verbose) message("Reading image.")

  if (!missing(img_object)) {
    if (any(c("RasterBrick", "RasterStack") %in% class(img_object))) {
      source_brick <- img_object
    } else {
      source_brick <- raster::brick(img_object)
    }
  } else {
    source_brick <- raster::brick(img_path)
  }

  # drop layers beyond rgb
  if (nlayers(source_brick) > 3) {
    for (i in seq_len(nlayers(source_brick) - 3)) {
      source_brick <- raster::dropLayer(source_brick, i + 3)
    }
  }

  # modify CRS if requested
  if (!is.null(img_xml_wkt_path) ||
      !is.null(wkt_string) ||
      !is.null(proj4_string)) {
    if (!is.null(img_xml_wkt_path)) {

      jp2_xml <- xml2::as_list(xml2::read_xml(jp2_aux_path))

      my_proj4_string <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string

    } else if (!is.null(wkt_string)) {

      my_proj4_string <- sf::st_crs(wkt = wkt_string)$proj4string

    } else if (!is.null(proj4_string)) {

      my_proj4_string <- proj4_string

    }

    if (reproject_raster == TRUE) {
      source_brick <- projectRaster(
        source_brick,
        res = res(source_brick),
        crs = my_proj4_string
      )
    } else {
      crs(source_brick) <- my_proj4_string
    }
  }

  # create raster extents and sf polygons for each tile
  slice_data <- calc_slice_corners(
    source_n_rows = nrow(source_brick),
    source_n_cols = ncol(source_brick),
    slice_n_rows = slice_n_rows,
    slice_n_cols = slice_n_cols,
    complete_image = complete_image
  )

  slice_data <- cbind(slice_id = seq_len(nrow(slice_data)),
                      slice_data,
                      stringsAsFactors = FALSE)

  if (!missing(img_path) & !is.null(img_xml_wkt_path)) {
    slice_data <- cbind(source_img = rep(img_path, nrow(slice_data)),
                        source_img_aux = rep(img_xml_wkt_path, nrow(slice_data)),
                        slice_data,
                        stringAsFactors = FALSE)
  } else if (!missing(img_path)) {
    slice_data <- cbind(source_img = rep(img_path, nrow(slice_data)),
                        slice_data,
                        stringAsFactors = FALSE)
  } else if (!missing(img_xml_wkt_path)) {
    slice_data <- cbind(source_img_aux = rep(img_xml_wkt_path, nrow(slice_data)),
                        slice_data,
                        stringAsFactors = FALSE)
  }

  slice_extents <- split(slice_data, 1:NROW(slice_data))

  if (verbose) message("Creating extents:")
  slice_data$extents <- pblapply(slice_extents, function(x) {
    extent(source_brick, x[["y0"]], x[["y1"]], x[["x0"]], x[["x1"]])
  })

  if (verbose) message("Creating polygons:")
  slice_data$geometry <- pblapply(slice_data$extents, function(x) {
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

  # create a 4d array for each tile, containing 1 [slice_n_rows, slice_n_cols, 3] slice
  if (verbose) message("Converting image to array and slicing - may take a minute.")
  source_brick_data <- raster::as.array(source_brick)

  slice_data$slice_array <- lapply(1:nrow(slice_data), function(i) {
    out <- array(NA, c(1, slice_n_cols, slice_n_rows, 3))

    out[1, , ,] <- source_brick_data[
      seq(slice_data[["y0"]][i], slice_data[["y1"]][i]),
      seq(slice_data[["x0"]][i], slice_data[["x1"]][i])
      ,
      ]

    out
  })

  # convert slice_data to sf object
  if (return_sf) {
    slice_data$geometry <- st_sfc(do.call("c", slice_data$geometry))
    slice_data <- st_sf(slice_data)
  }

  # # must transpose each layer to reassemble original brick
  # temp <- crop(source_brick, slice_data$extents[[1]])
  # plotRGB(temp)
  #
  # temp_array <- drop(slice_data$slice_array[[1]])
  # temp_array[,,1] <- t(temp_array[,,1])
  # temp_array[,,2] <- t(temp_array[,,2])
  # temp_array[,,3] <- t(temp_array[,,3])
  # plotRGB(temp)

  slice_data
}