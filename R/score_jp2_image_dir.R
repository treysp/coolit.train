#' Score a directory of jp2 images with a Keras model
#'
#' @param jp2_dir Path to directory containing .jp2 image files
#' @param jp2_aux_dir Path to directory containing .jp2.aux.xml files with
#'                    metadata about .jp2 image with corresponding name
#'
#' @param model_h5_weights Saved h5 weights from a trained keras model, used to score
#'                         the image tiles.
#' @param model_params_dput_file Path to file containing \code{dput} export of parameters
#' used during training of the model contained in `model_h5_weights`.
#'
#' @param score_outdir Path to directory where results of image scoring should be save
#' @param compress_score_rds Should saved score results be compressed
#' @param return_score Should list of score results be returned as an R object by the function
#'
#' @param tile_overlap Number of pixel overlap in adjacent tiles (in both X and Y directions).
#'                       See \code{\link{calc_slice_corners}}.
#'
#' @param complete_image See \code{\link{calc_slice_corners}}.
#'
#' @param verbose Should messages about current step being processes be printed to screen?
#'
#' @return List of data frame with one row for each tile, containing:
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
#' If score \code{outpath != NULL} each returned data frame will be saved to outpath as an
#' RDS file, compressed if \code{compress_score_rds == TRUE}.
#'
#' @export
#' @importFrom pbapply pblapply
#' @importFrom keras load_model_hdf5
score_jp2_image_dir <- function(jp2_dir, jp2_aux_dir,
                                model_params_dput_file, model_h5_weights,
                                score_outdir = NULL, compress_score_rds = FALSE,
                                return_score = TRUE, verbose = FALSE,
                                tile_overlap = 0, complete_image = FALSE) {
  jp2_file_names <- make_jp2_file_df(jp2_dir, jp2_aux_dir)

  images_to_score <- split(jp2_file_names, 1:nrow(jp2_file_names))

  # load model and score
  my_model_params <- eval(parse(model_params_dput_file))
  scoring_model <- load_model_hdf5(model_h5_weights)

  out <- pblapply(images_to_score, function(x) {

    score_jp2_image(
      model_params = my_model_params,
      scoring_model = scoring_model,
      jp2_path = x[["img_file"]],
      jp2_aux_path = x[["aux_file"]],
      score_outpath = file.path(score_outdir, paste0("img_", x[["stub"]], "_scores.rds")),
      return_score = return_score,
      verbose = verbose,
      tile_overlap = tile_overlap,
      complete_image = complete_image
    )

  })

  out
}