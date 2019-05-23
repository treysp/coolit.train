#' Score a jp2 image with a Keras model
#'
#' @param model_params Model parameters object created during model training
#' @param scoring_model Trained keras model object
#'
#' @param jp2_path Path to .jp2 image files
#' @param jp2_aux_path Path to .jp2.aux.xml file containing metadata about .jp2 image
#'
#' @param score_outpath Path to save results of image scoring as .rds file
#' @param compress_score_rds Should saved score results be compressed
#' @param return_score Should score results be returned as an R object by the function
#'
#' @param tile_overlap Number of pixel overlap in adjacent tiles (in both X and Y directions).
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
#' \[1, \code{tile_n_cols}, \code{tile_n_rows}, 3\]
#' - Model predicted probability for each image tile
#'
#' If score \code{outpath != NULL} returned data frame will be saved to outpath as an
#' RDS file, compressed if \code{compress_score_rds == TRUE}.
#'
#' @export
#' @importFrom abind abind
#' @importFrom keras predict_proba
score_jp2_image <- function(model_params, scoring_model,
                            jp2_path, jp2_aux_path,
                            score_outpath = NULL, compress_score_rds = FALSE,
                            return_score = TRUE, verbose = FALSE,
                            tile_overlap = 0, complete_image = FALSE) {
  params <- as.list(environment())
  model_params$scoring_params <- params

  # prep tiles
  tile_data <- slice_jp2_image(
    jp2_path = jp2_path,
    jp2_aux_path = jp2_aux_path,
    tile_n_rows = model_params$img_size[1],
    tile_n_cols = model_params$img_size[2],
    tile_overlap = tile_overlap,
    complete_image = complete_image,
    verbose = verbose
  )

  tile_array <- abind::abind(tile_data$tile_array, along = 1)
  tile_array <- tile_array / 255

  # score tiles
  tile_data$predicted_probs <- predict_proba(scoring_model, tile_array)

  # save
  if (!is.null(score_outpath)) {
    saveRDS(list(scoring_params = model_params, tile_data = tile_data),
            file = score_outpath,
            compress = compress_score_rds)
  }

  if (!return_score) {
    return(NULL)
  } else {
    return(tile_data)
  }
}