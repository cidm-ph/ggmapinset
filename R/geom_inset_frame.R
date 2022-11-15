#' Add a frame and burst lines for an inset.
#'
#' @section Controlling aesthetics:
#' Aesthetic mappings or parameters can be provided. They can be scalars, or
#' vectors of length 4 (times the number of panels when faceting) corresponding
#' in order to the source circle, target circle, then the two line segments
#' connecting them, respectively. Note that the connecting lines might not be
#' drawn if the inset and original circle overlap, which will affect the count.
#'
#' @inheritParams geom_sf_inset
#' @export
geom_inset_frame <- function(mapping = ggplot2::aes(),
                             data = NULL,
                             stat = "sf", position = "identity",
                             ...,
                             inset = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = FALSE) {
  if (!is.null(inset) & !is_inset_config(inset)) {
    cli::cli_abort(c("{.arg inset} must be a {.class inset_config} object",
                     "i" = "See {.fn configure_inset}"))
  }

  params <- rlang::list2(na.rm = na.rm, ...)
  if (is.null(mapping[["colour"]]) & is.null(params[["colour"]])) {
    params$colour <- "#bbbbbb"
  }
  if (is.null(mapping[["fill"]]) & is.null(params[["fill"]])) {
    params$fill <- NA
  }
  if (is.null(mapping[["linewidth"]]) & is.null(params[["linewidth"]])) {
    params$linewidth <- 0.4
  }

  data <- make_burst_circle(inset)

  ggplot2::layer_sf(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomSf,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

make_burst_circle <- function (inset) {
  crs_working <- inset_crs_working(inset)
  crs_orig <- sf::st_crs(inset_centre(inset))

  centroid <- sf::st_transform(inset_centre(inset), crs_working)
  trans <- inset_translation(inset)
  if (is.null(trans)) trans <- c(0, 0)
  scale <- inset_scale(inset)
  if (is.null(scale)) scale <- 1
  radius <- inset_radius(inset)

  viewport <- sf::st_buffer(centroid, radius)
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
  sf::st_sf(c(viewport, result, lines))
}

get_outer_bitangents <- function (centre1, radius1, centre2, radius2) {
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

  if (hypot < short) return(sf::st_sfc()) # one circle fully inside the other
  # if (hypot == short)     the lines are degenerate
  # otherwise               there are 2 external bitangents

  phi1 <- atan2(centre2[[2]] - centre1[[2]], centre2[[1]] - centre1[[1]]) + acos(short / hypot)
  b1 <- sf::st_sfc(
    sf::st_point(c(centre1[[1]] + radius1 * cos(phi1), centre1[[2]] + radius1 * sin(phi1))),
    sf::st_point(c(centre2[[1]] + radius2 * cos(phi1), centre2[[2]] + radius2 * sin(phi1))))
  b1 <- sf::st_cast(sf::st_union(b1), "LINESTRING")

  phi2 <- atan2(centre2[[2]] - centre1[[2]], centre2[[1]] - centre1[[1]]) - acos(short / hypot)
  b2 <- sf::st_sfc(
    sf::st_point(c(centre1[[1]] + radius1 * cos(phi2), centre1[[2]] + radius1 * sin(phi2))),
    sf::st_point(c(centre2[[1]] + radius2 * cos(phi2), centre2[[2]] + radius2 * sin(phi2))))
  b2 <- sf::st_cast(sf::st_union(b2), "LINESTRING")

  c(b1, b2)
}
