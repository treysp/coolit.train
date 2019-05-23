#' make overlay of towers on image
#'
#' @param base_raster Either a path to a jp2 image with paired .aux.xml file
#' or an existing rasterBrick or rasterStack object with valid CRS
#'
#' @param red_polygon_sf SF data frame containing polygons to overlay on the
#' raster in the red layer
#'
#' @param green_polygon_sf SF data frame containing polygons to overlay on the
#' raster in the green layer
#'
#' @param blue_polygon_sf SF data frame containing polygons to overlay on the
#' raster in the blue layer
#'
#' @param img_xml_wkt_path Optional path to .xml file containing metadata about
#'                         image in WKT format.
#'
#' @param outfilename Either file path for output raster or NULL
#'
#' @param write_only TRUE for only saving raster to file, FALSE for returning
#' overlayed raster object
#'
#' @importFrom raster brick dropLayer crs plotRGB
#' overlay writeRaster
#' @importFrom sf st_crs
#' @importFrom xml2 as_list read_xml
#' @importFrom lwgeom st_transform_proj
#' @importFrom fasterize fasterize
#' @export
make_raster_overlay <- function(base_raster,
                                red_polygon_sf = NULL,
                                green_polygon_sf = NULL,
                                blue_polygon_sf = NULL,
                                img_xml_wkt_path = NULL,
                                outfilename = NULL,
                                write_only = FALSE) {
  if (is.null(red_polygon_sf) & is.null(blue_polygon_sf) &
      is.null(green_polygon_sf)) {
    stop("One of the [red|green|blue]_polygon_sf arguments must be non-NULL.")
  }

  if (!("raster" %in% class(base_raster)) &&
      is.character(base_raster) &&
      length(base_raster) == 1) {

    base_raster_path <- base_raster

    base_raster <- brick(base_raster_path)

    if (nlayers(base_raster) > 3) {
      for (i in seq_len(nlayers(base_raster) - 3)) {
        base_raster <- raster::dropLayer(base_raster, i + 3)
      }
    }

    if (!is.null(img_xml_wkt_path)) {
      jp2_xml <- xml2::as_list(xml2::read_xml(img_xml_wkt_path))

      crs(base_raster) <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string
    }
  }

  # rasterize
  my_rasterize <- function(polygon_sf, raster_value, layer_num) {
    polygon_sf <- lwgeom::st_transform_proj(polygon_sf,
                                            crs = crs(base_raster)@projargs)

    polygon_sf$cell_val <- raster_value
    overlay_raster <- fasterize(polygon_sf, base_raster[[layer_num]], "cell_val")

    base_raster[[layer_num]] <- overlay(base_raster[[layer_num]],
                                overlay_raster,
                                fun = function(x, y) pmax(x, y, na.rm = TRUE))

    base_raster
  }

  if (!is.null(red_polygon_sf)) {
    message("Red go\n")
    base_raster <- my_rasterize(red_polygon_sf, 250, 1)
  }

  if (!is.null(green_polygon_sf)) {
    message("Green go\n")
    base_raster <- my_rasterize(green_polygon_sf, 245, 2)
  }

  if (!is.null(blue_polygon_sf)) {
    message("Blue go\n")
    base_raster <- my_rasterize(blue_polygon_sf, 250, 3)
  }

  plotRGB(base_raster)

  if (!is.null(outfilename)) {
    writeRaster(
      base_raster,
      outfilename,
      datatype = "INT1U",
      overwrite = TRUE
    )
  }

  if (write_only) {
    return(NULL)
  } else {
    return(base_raster)
  }
}