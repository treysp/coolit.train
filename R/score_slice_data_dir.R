#' Score a directory of tile data files with a Keras model
#'
#' @param slice_data_dir Directory containing sliced image .rds files
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
#' @param verbose Should messages about current step being processes be printed to screen?
#'
#' @return List of data frame with one row for each tile, containing:
#' List of lists containing model parameters and input `img_data` data frame with
#' additional column containing model predicted probabilities for each slice
#'
#' If \code{score_outdir != NULL} each list will be saved to score_outdir as an
#' RDS file, compressed if \code{compress_score_rds == TRUE}.
#'
#' @export
#' @importFrom pbapply pblapply
#' @importFrom keras load_model_hdf5
#' @importFrom stringr str_match
score_slice_data_dir <- function(slice_data_dir,
                                 model_params_dput_file, model_h5_weights,
                                 score_outdir = NULL, compress_score_rds = FALSE,
                                 return_score = TRUE, keep_array = FALSE, verbose = FALSE) {
  slice_data_files <- list.files(slice_data_dir, full.names = TRUE, recursive = TRUE)

  images_to_score <- split(slice_data_files, 1:length(slice_data_files))

  # load model and score
  my_model_params <- eval(parse(model_params_dput_file))
  scoring_model <- load_model_hdf5(model_h5_weights)

  out <- pblapply(images_to_score, function(x) {

    slice_data <- readRDS(x)

    score_outname <- stringr::str_match(x, "(.*/)(.*?)\\.rds$")[, 3]

    score_slice_data(
      slice_data = slice_data,
      model_params = my_model_params,
      scoring_model = scoring_model,
      score_outpath = file.path(score_outdir, paste0(score_outname, "_scores.rds")),
      return_score = return_score,
      keep_array = keep_array,
      verbose = verbose
    )

  })

  out
}