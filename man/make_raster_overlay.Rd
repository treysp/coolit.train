% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_raster_overlay.R
\name{make_raster_overlay}
\alias{make_raster_overlay}
\title{make overlay of towers on image}
\usage{
make_raster_overlay(base_raster, red_polygon_sf = NULL,
  green_polygon_sf = NULL, blue_polygon_sf = NULL,
  img_xml_wkt_path = NULL, outfilename = NULL, write_only = FALSE)
}
\arguments{
\item{base_raster}{Either a path to a jp2 image with paired .aux.xml file
or an existing rasterBrick or rasterStack object with valid CRS}

\item{red_polygon_sf}{SF data frame containing polygons to overlay on the
raster in the red layer}

\item{green_polygon_sf}{SF data frame containing polygons to overlay on the
raster in the green layer}

\item{blue_polygon_sf}{SF data frame containing polygons to overlay on the
raster in the blue layer}

\item{img_xml_wkt_path}{Optional path to .xml file containing metadata about
image in WKT format.}

\item{outfilename}{Either file path for output raster or NULL}

\item{write_only}{TRUE for only saving raster to file, FALSE for returning
overlayed raster object}
}
\description{
make overlay of towers on image
}
