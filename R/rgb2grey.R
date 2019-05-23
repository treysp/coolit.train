#' Convert RGB to greyscale using 'average' or 'gleam' methods
#'
#' @param my_brick Object of class RasterBrick or RasterStack from the raster package
#' @param method Method of converting RGB to greyscale
#'
#' @details
#' For the 'gleam' method we assume that the RGB image has been gamma-compressed.
#' We then:
#' 1. Gamma expand it based on the formula at the wikipedia link below
#' 2. Calculate the arithmetic mean across the gamma-expanded channels
#' 3. Rescale to 0:255
#'
#' See PLoS One article referenced below.
#'
#' @aliases rgb2gray
#' @references
#' \url{https://en.wikipedia.org/wiki/Grayscale#Converting_color_to_grayscale}
#' \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0029740}
rgb2grey <- function(my_brick, method = c("ave", "gleam")) {
  if (length(method) > 1) {
    warning("Method unspecified, using `ave`.")
  }

  if (method == "gleam") {
    for (i in 1:3) {
      temp_layer <- raster::getValues(my_brick[[i]])
      temp_layer <- temp_layer / 255

      ifelse(temp_layer <= 0.04045, {
        temp_layer <- temp_layer / 12.92
      }, {
        temp_layer <- ((temp_layer + 0.055) / 1.055) ^ 2.2
      })

      my_brick[[i]] <- temp_layer
    }

    temp_gamma_grey <- raster::calc(my_brick, fun = mean)
    temp_gamma_grey_values <- raster::getValues(temp_gamma_grey)

    ifelse(temp_gamma_grey_values <= 0.0031308, {
      temp_gamma_grey_values <- temp_gamma_grey_values * 12.92
    }, {
      temp_gamma_grey_values <- ((temp_gamma_grey_values ^ (1/2.2)) * 1.055) - 0.055
    })

    temp_gamma_grey <- raster::setValues(temp_gamma_grey, temp_gamma_grey_values)
    temp_gamma_grey <- raster::calc(temp_gamma_grey, fun = function(x) x * 255)
  } else {
    temp_gamma_grey <- raster::calc(my_brick, fun = mean)
  }

  temp_gamma_grey
}