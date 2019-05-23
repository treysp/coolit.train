#' Score a data frame of image slices with a Keras model
#'
#' @param slice_data Data frame containing image slice data
#'
#' @param model_params Model parameters object created during model training
#' @param scoring_model Trained keras model object
#'
#' @param score_outpath Path to save results of image scoring as .rds file
#' @param compress_score_rds Should saved score results be compressed
#' @param return_score Should score results be returned as an R object by the function
#'
#' @param verbose Should messages about current step being processes be printed to screen?
#'
#' @return
#' List containing model parameters and input `img_data` data frame with
#' additional column containing model predicted probabilities for each slice
#'
#' If score \code{outpath != NULL} returned data frame will be saved to outpath as an
#' RDS file, compressed if \code{compress_score_rds == TRUE}.
#'
#' @export
#' @importFrom abind abind
#' @importFrom keras predict_proba
score_slice_data <- function(slice_data, model_params, scoring_model,
                             score_outpath = NULL, compress_score_rds = FALSE,
                             return_score = TRUE, keep_array = FALSE, verbose = FALSE) {
  model_params$scoring_params <- list(
    score_outpath = score_outpath, compress_score_rds = compress_score_rds,
    return_score = return_score, keep_array = keep_array, verbose = verbose
    )

  slice_array <- abind::abind(slice_data$slice_array, along = 1)
  slice_array <- slice_array / 255

  # score tiles
  slice_data$predicted_probs <- predict_proba(scoring_model, slice_array)

  if (!keep_array) {
    slice_data$slice_array <- NULL
  }

  # save
  if (!is.null(score_outpath)) {
    saveRDS(list(scoring_params = model_params, slice_data = slice_data),
            file = score_outpath,
            compress = compress_score_rds)
  }

  if (!return_score) {
    return(NULL)
  } else {
    return(slice_data)
  }
}