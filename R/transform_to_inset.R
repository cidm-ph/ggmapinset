#' Transform coordinates according to inset configuration
#'
#' This helper operates on an sf object to scale and translate its objects according
#' to the inset specification.
#'
#' @param x Spatial data frame or other sf object; see [sf::st_geometry()].
#' @param inset Inset configuration; see [configure_inset()].
#'
#' @returns A copy of `x` with the geometry replaced by the transformed version.
#' @export
#' @examples
#' library(sf)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' cfg <- configure_inset(
#'   centre = st_sfc(st_point(c(-82, 35)), crs = 4326),
#'   scale = 2,
#'   translation = c(10, -60),
#'   radius = 50,
#'   units = "mi")
#'
#' transform_to_inset(nc, cfg)
transform_to_inset <- function(x, inset) {
  geometry <- sf::st_geometry(x)
  inset <- make_inset_config(inset)

  crs_orig <- sf::st_crs(geometry)
  crs_working <- inset_crs_working(inset)

  centre <- sf::st_transform(inset_centre(inset), crs_working)
  scale <- inset_scale(inset)
  translation <- inset_translation(inset)

  result <- sf::st_transform(geometry, crs_working)
  if (!is.null(scale)) {
    result <- (result - centre) * scale + centre
    result <- sf::st_set_crs(result, crs_working)
  }
  if (!is.null(translation)) {
    result <- sf::st_set_crs(result + translation, crs_working)
  }
  result <- sf::st_transform(result, crs_orig)
  sf::st_set_geometry(x, result)
}
