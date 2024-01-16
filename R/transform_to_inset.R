#' Transform coordinates according to inset configuration
#'
#' This helper operates on an sf object to scale and translate its geometry according
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
  crs_working <- inset_crs_working(inset)
  scale <- inset_scale(inset)
  translation <- inset_translation(inset)

  result <- with_crs_working(
    crs_working, geometry, inset_centre(inset),
    .f = function(result, centre) {
      transform(result, centre, scale = scale, translation = translation)
    }
  )

  if (has_s3_method("st_geometry<-", class(x))) {
    sf::st_set_geometry(x, result)
  } else {
    result
  }
}

has_s3_method <- function(f, classes) {
  any(mapply(FUN = function(c) !is.null(utils::getS3method(f, c, optional = TRUE)), classes))
}
