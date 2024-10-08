#' @export
inset_viewport.inset_shape_rectangle <- function(inset) {
  centre <- sf::st_transform(inset_centre(inset), inset_crs_working(inset))

  width <- inset_width(inset)
  height <- inset_height(inset)
  c(
    centre + c(-width, -height),
    centre + c(-width, +height),
    centre + c(+width, +height),
    centre + c(+width, -height),
    centre + c(-width, -height)
  ) |>
    sf::st_combine() |>
    sf::st_cast("POLYGON") |>
    sf::st_set_crs(sf::st_crs(centre))
}

#' @export
make_frame.inset_shape_rectangle <- function(inset) {
  crs_working <- inset_crs_working(inset)
  crs_orig <- sf::st_crs(inset_centre(inset))

  centroid <- sf::st_transform(inset_centre(inset), crs_working)
  trans <- inset_translation(inset)
  if (is.null(trans)) trans <- c(0, 0)
  scale <- inset_scale(inset)
  if (is.null(scale)) scale <- 1
  width <- inset_width(inset)
  height <- inset_width(inset)

  viewport <- inset_viewport(inset)
  result <- viewport

  if (scale != 1) {
    result <- (result - centroid) * scale + centroid
    result <- sf::st_set_crs(result, crs_working)
  }
  if (!is.null(inset_translation(inset))) {
    result <- sf::st_set_crs(result + trans, crs_working)
  }

  # lines <- get_outer_bitangents(centroid[[1]], radius, centroid[[1]] + trans, radius * scale)
  lines <- c(sf::st_sfc(sf::st_linestring()), sf::st_sfc(sf::st_linestring()))
  lines <- sf::st_set_crs(lines, crs_working)
  lines <- sf::st_transform(lines, crs_orig)

  viewport <- sf::st_transform(viewport, crs_orig)
  result <- sf::st_transform(result, crs_orig)
  c(viewport, result, lines)
}
