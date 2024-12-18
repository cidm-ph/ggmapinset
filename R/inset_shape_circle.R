#' Circular insets
#'
#' @param centre Coordinates of the inset centre. Ideally this should be an
#'   \code{sfc} object (see [sf::st_sfc()]) including a coordinate reference system.
#'   An [sf::st_point()] or a vector of longitude and latitude are also accepted.
#'   If a CRS cannot be determined, WGS 84 is assumed.
#' @param radius Radius of the inset circle in the units of the inset's `crs_working`.
#' @family shapes
#' @seealso [configure_inset()]
#' @export
#'
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' make_demo <- function(...) {
#'   ggplot(nc) +
#'     geom_sf(fill = "grey95", colour = "grey85") +
#'     # For a filled frame, we want to interleave it between the base layer
#'     # (above this line) and the target layer (below the following line).
#'     geom_inset_frame(target.aes = list(fill = "white")) +
#'     geom_sf_inset(map_base = "none") +
#'     coord_sf_inset(inset = configure_inset(...)) +
#'     theme_void()
#' }
#' centroid <-
#'   sf::st_centroid(nc$geometry[[21]]) |>
#'   sf::st_sfc(crs = sf::st_crs(nc))
#' circle <- shape_circle(centroid, radius = 50)
#'
#' make_demo(circle, scale = 3, translation = c(-200, -200))
#' make_demo(circle, scale = 3, translation = c(-100, -100))
#' make_demo(circle, scale = 3, translation = c(0, 0))
#' make_demo(circle, scale = 0.5, translation = c(0, 0))
shape_circle <- function(centre, radius) {
  centre <- coerce_centre(centre)
  if (radius <= 0) {
    cli::cli_abort("Circle {.arg radius} must be a positive number, not {radius}")
  }

  structure(
    list(centre = centre, radius = radius),
    class = c("shape_circle", "ggmapinset_shape")
  )
}

#' @export
central_point.shape_circle <- function(shape) {
  shape$centre
}

#' @export
inset_viewport.inset_shape_circle <- function(inset) {
  centre <- sf::st_transform(inset_centre(inset), inset_crs_working(inset))
  sf::st_buffer(centre, inset_shape(inset)$radius)
}

#' @export
make_frame.inset_shape_circle <- function(inset) {
  crs_working <- inset_crs_working(inset)
  crs_orig <- sf::st_crs(inset_centre(inset))

  centroid <- sf::st_transform(inset_centre(inset), crs_working)
  trans <- inset_translation(inset)
  if (is.null(trans)) trans <- c(0, 0)
  scale <- inset_scale(inset)
  if (is.null(scale)) scale <- 1
  radius <- inset_shape(inset)$radius

  viewport <- inset_viewport(inset)
  result <- viewport

  if (scale != 1) {
    result <- (result - centroid) * scale + centroid
    result <- sf::st_set_crs(result, crs_working)
  }
  if (!is.null(inset_translation(inset))) {
    result <- sf::st_set_crs(result + trans, crs_working)
  }

  lines <- get_outer_bitangents(centroid[[1]], radius, centroid[[1]] + trans, radius * scale)
  lines <- sf::st_set_crs(lines, crs_working)
  lines <- sf::st_transform(lines, crs_orig)

  viewport <- sf::st_transform(viewport, crs_orig)
  result <- sf::st_transform(result, crs_orig)
  c(viewport, result, lines)
}

get_outer_bitangents <- function(centre1, radius1, centre2, radius2) {
  if (radius2 > radius1) {
    tmp1 <- radius2
    tmp2 <- centre2
    radius2 <- radius1
    centre2 <- centre1
    radius1 <- tmp1
    centre1 <- tmp2
  }

  hypot <- sqrt((centre2[[1]] - centre1[[1]])^2 + (centre2[[2]] - centre1[[2]])^2)
  short <- radius1 - radius2

  if (hypot < short) {
    return(sf::st_multilinestring() |> sf::st_sfc(crs = sf::st_crs(centre1)))
  }
  # if (hypot < short) return(sf::st_sfc()) # one circle fully inside the other
  # if (hypot == short)     the lines are degenerate
  # otherwise               there are 2 external bitangents

  phi1 <- atan2(centre2[[2]] - centre1[[2]], centre2[[1]] - centre1[[1]]) + acos(short / hypot)
  b1 <- sf::st_sfc(
    sf::st_point(c(centre1[[1]] + radius1 * cos(phi1), centre1[[2]] + radius1 * sin(phi1))),
    sf::st_point(c(centre2[[1]] + radius2 * cos(phi1), centre2[[2]] + radius2 * sin(phi1)))
  )
  b1 <- sf::st_cast(sf::st_union(b1), "LINESTRING")

  phi2 <- atan2(centre2[[2]] - centre1[[2]], centre2[[1]] - centre1[[1]]) - acos(short / hypot)
  b2 <- sf::st_sfc(
    sf::st_point(c(centre1[[1]] + radius1 * cos(phi2), centre1[[2]] + radius1 * sin(phi2))),
    sf::st_point(c(centre2[[1]] + radius2 * cos(phi2), centre2[[2]] + radius2 * sin(phi2)))
  )
  b2 <- sf::st_cast(sf::st_union(b2), "LINESTRING")

  sf::st_combine(c(b1, b2))
}
