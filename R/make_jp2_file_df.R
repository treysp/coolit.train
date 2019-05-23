#' Create data frame of jp2 and jp2 aux files from directory paths
#'
#' @param jp2_dir Path to directory containing .jp2 image files
#' @param jp2_aux_dir Path to directory containing .jp2.aux.xml files with
#'                    metadata about .jp2 image with corresponding name
#'
#' @export
#' @importFrom stringr str_match
make_jp2_file_df <- function(jp2_dir, jp2_aux_dir) {

  # make paired list of img and aux files
  jp2_files <-
    list.files(jp2_dir, full.names = TRUE, recursive = TRUE)
  jp2_files <- jp2_files[grepl("\\.jp2$", jp2_files)]

  jp2_stub <- stringr::str_match(jp2_files, "(.*/)(.*?)\\.jp2$")
  jp2_stub <- jp2_stub[, ncol(jp2_stub)]

  if (any(duplicated(jp2_stub))) {
    warning(
      "Duplicated jp2 file stubs found. ",
      "Only scoring first file for each duplicated stub.\n"
    )
  }

  jp2_file_names <- data.frame(stub = jp2_stub,
                               img_file = jp2_files,
                               stringsAsFactors = FALSE)

  jp2_file_names <- jp2_file_names[!duplicated(jp2_file_names$stub), ]

  jp2_aux_files <-
    list.files(jp2_aux_dir, full.names = TRUE, recursive = TRUE)
  jp2_aux_files <-
    jp2_aux_files[grepl("\\.jp2\\.aux\\.xml$", jp2_aux_files)]

  aux_stub <-
    str_match(jp2_aux_files, "(.*/)(.*?)\\.jp2\\.aux\\.xml$")
  aux_stub <- aux_stub[, ncol(aux_stub)]

  if (any(duplicated(aux_stub))) {
    warning(
      "Duplicated jp2 aux file stubs found. ",
      "Only scoring first file for each duplicated stub.\n"
    )
  }

  jp2_aux_files <- data.frame(stub = aux_stub,
                              aux_file = jp2_aux_files,
                              stringsAsFactors = FALSE)

  jp2_aux_files <- jp2_aux_files[!duplicated(jp2_aux_files$stub), ]

  jp2_file_names <- merge(jp2_file_names, jp2_aux_files, by = "stub")

  if (length(jp2_stub) != length(aux_stub)) {
    warning("Only scoring images with a corresponding aux file.\n")
  }

  jp2_file_names <- jp2_file_names[!is.na(jp2_file_names$img_file) &
                                     !is.na(jp2_file_names$aux_file)
                                   ,]

  jp2_file_names
}