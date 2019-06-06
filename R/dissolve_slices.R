#' dissolve adjacent tower slices into polygons or polygon centroid points
#'
#' @param tower_sf sf data frame object containing columns `img_id` and `slice_id`
#'                   that uniquely identify each slice
#' @param out_geometry should output be dissolved polygons or the centroids of
#'                       the dissolved polygons
#' @param out_name optional name of file to save dissolved geomtery. must have file
#'                   extension writable by \code{\link[sf]{st_write}}
#'
#' @return sf data frame containing dissolved geometry of output type `out_geometry`
#' @export
#' @importFrom sf st_intersects st_convex_hull st_union st_centroid st_write
#' @importFrom igraph graph_from_adjacency_matrix components
#' @importFrom stringr str_match
dissolve_slices <- function(tower_sf, out_geometry = c("polygon", "centroid"),
                            out_name = NULL) {
  if (!inherits(tower_sf, "sf")) {
    stop("`tower_sf` must be a valid sf data frame object")
  }

  if (!("img_id" %in% names(tower_sf) ||
        "slice_id" %in% names(tower_sf))) {
    stop("`tower_sf` must have columns 'img_id' and 'slice_id'")
  }

  if (length(out_geometry) != 1 ||
      !(out_geometry %in% c("polygon", "centroid"))) {
    stop("`out_geometry` must be one of 'polygon' or 'centroid'")
  }

  tower_intersects <- tower_sf$geometry

  tower_intersects_mat <- st_intersects(tower_intersects, sparse = FALSE)
  rownames(tower_intersects_mat) <- paste0(tower_sf$img_id, "_",
                                           tower_sf$slice_id)
  colnames(tower_intersects_mat) <- paste0(tower_sf$img_id, "_",
                                           tower_sf$slice_id)

  tower_igraph <- igraph::graph_from_adjacency_matrix(
    tower_intersects_mat,
    mode = "undirected",
    diag = FALSE)

  tower_igraph_comp <- igraph::components(tower_igraph)

  slice_comp <- data.frame(
      temp_id = names(tower_igraph_comp$`membership`),
      comp_num = tower_igraph_comp$`membership`,
      comp_size = rep(tower_igraph_comp$csize, times = tower_igraph_comp$csize),
      stringsAsFactors = FALSE
      )

  slice_comp$img_id <- stringr::str_match(
        temp_id, "(\\d{1,4})_(\\d{1,4})")[, 2]

  slice_comp$slice_id <- stringr::str_match(
        temp_id, "(\\d{1,4})_(\\d{1,4})")[, 3]

  slice_comp$temp_id <- NULL

  tower_sf$img_id <- as.character(tower_sf$img_id)
  tower_sf$slice_id <- as.character(tower_sf$slice_id)

  tower_sf <- merge(tower_sf,
                    slice_comp,
                    by = c("img_id", "slice_id"))

  solo_slices <- tower_sf[tower_sf$comp_size == 1, ]

  dissolved_slices <- tower_sf[tower_sf$comp_size > 1, ]
  dissolved_slices <- split(dissolved_slices, dissolved_slices$comp_num)

  dissolved_slices <- lapply(dissolved_slices, function(x) {
    img_id <- unique(x$img_id)

    slice_id <- paste0(x$slice_id, collapse = "_")

    temp_poly <- st_convex_hull(st_union(x$geometry))

    st_sf(img_id = img_id, slice_id = slice_id, geometry = temp_poly)
  })

  dissolved_slices <- do.call("rbind", dissolved_slices)

  tower_slices <- rbind(solo_slices[, c("img_id", "slice_id", "geometry")],
                        dissolved_slices)

  if (out_geometry == "centroid") {
    tower_slices_point <- tower_slices
    tower_slices_point$geometry <- st_centroid(tower_slices_point$geometry)

    if (!is.null(out_name)) {
      st_write(tower_slices_point, out_name)
    }

    return(tower_slices_point)
  } else {
    if (!is.null(out_name)) {
      st_write(tower_slices, out_name)
    }

    return(tower_slices)
  }
}