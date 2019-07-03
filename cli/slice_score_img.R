#!/usr/local/bin/Rscript

#library(argparser)
#library(coolit.train)
library(stringr)
library(rgdal)
library(keras)

# p <- arg_parser("Slice and score an image")
#
# p <- add_argument(p,
#                   arg = "inpath",
#                   help = "Path to image file",
#                   type = "character",
#                   nargs = 1)
#
# p <- add_argument(p,
#                   arg = "save_slices",
#                   help = "Should slices be saved to file?",
#                   default = FALSE,
#                   flag = TRUE)
#
# p <- add_argument(p,
#                   arg = "--slice_outpath",
#                   help = "Path and name of .rds file to which slices should be saved",
#                   type = "character",
#                   nargs = 1)
#
# p <- add_argument(p,
#                   arg = "score_outdir",
#                   help = "Directory to which .rds scores file should be saved",
#                   type = "character",
#                   nargs = 1)
#
# p <- add_argument(p,
#                   arg = "model_params_dput_file",
#                   help = "Path to .txt file containing dput contents of model params",
#                   type = "character",
#                   nargs = 1)
#
# p <- add_argument(p,
#                   arg = "model_h5_weights",
#                   help = "Path to .h5 weights file corresponding model with dput model params",
#                   type = "character",
#                   nargs = 1)
#
# p <- add_argument(p,
#                   arg = "keep_array",
#                   help = "Should saved scores file contain arrays of each slice image?",
#                   default = FALSE,
#                   flag = TRUE)
#
# p <- add_argument(p,
#                   arg = "compress_score_rds",
#                   help = "Should saved scores file be compressed?",
#                   default = FALSE,
#                   flag = TRUE)
#
# args <- pars_args(p)

if (args$save_slices == TRUE &&
    (is.na(args$slice_outpath) || str_trim(args$slice_outpath) == "")) {
  stop("If save_slices == TRUE then slice_outpath must have a valid value.")
}

img_stub <- str_match(args$inpath, "(.*/)*(.*)\\..{3}$")[, 3]

my_proj4 <- rgdal::GDALinfo(args$inpath)
my_proj4 <- attr(my_proj4, "projection")

slice_data <- slice_image(
  img_path = args$inpath,
  slice_n_rows = 50,
  slice_n_cols = 50,
  slice_overlap = 0,
  complete_image = TRUE,
  proj4_string = my_proj4,
  verbose = FALSE
)

if (args$save_slices) {
  saveRDS(slice_data,
          file.path(args$slice_outpath, paste0(img_stub, "_slices.rds")),
          compress = FALSE)
}

my_model_params <- eval(parse(args$model_params_dput_file))
scoring_model <- load_model_hdf5(args$model_h5_weights)

score_slice_data(
  slice_data = slice_data,
  model_params = my_model_params,
  scoring_model = scoring_model,
  score_outpath = file.path(args$score_outdir, paste0(img_stub, "_slices_scores.rds")),
  return_score = FALSE,
  keep_array = args$keep_array,
  compress_score_rds = args$compress_score_rds,
  verbose = verbose
)
