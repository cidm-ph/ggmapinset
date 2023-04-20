#' Add a frame and burst lines for an inset
#'
#' @section Limitation:
#' The frame cannot be drawn without another sf layer that contains data due to
#' a limitation of the ggplot layout evaluation. Attempting to plot a frame by
#' itself will result in the error:
#' "Scale limits cannot be mapped onto spatial coordinates in `coord_sf()`".
#'
#' @param source.aes,target.aes,lines.aes Override the aesthetics of the
#'   inset source, target, and lines respectively. The value should be a list
#'   named by the aesthetics, and the values should be scalars of length one.
#' @inheritParams geom_sf_inset
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,...
#'   See [ggplot2::geom_sf()].
#'
#' @returns A ggplot layer holding the inset frame.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' ggplot(nc) +
#'   geom_sf_inset() +
#'   geom_inset_frame() +
#'   coord_sf_inset(inset = configure_inset(
#'     centre = sf::st_sfc(sf::st_point(c(-82, 35)), crs = 4326),
#'     scale = 2, translation = c(0, -300), radius = 50, units = "mi"))
geom_inset_frame <- function(mapping = ggplot2::aes(),
                             data = NULL,
                             stat = "sf_inset", position = "identity",
                             ...,
                             inset = NA,
                             na.rm = FALSE,
                             source.aes = list(),
                             target.aes = list(),
                             lines.aes = list(),
                             show.legend = NA,
                             inherit.aes = FALSE) {
  if (!is.null(data)) {
    cli::cli_warn("Ignoring {.arg data} provided to {.fn geom_inset_frame} layer")
  }
  params <- rlang::list2(na.rm = na.rm, source.aes = source.aes,
                         target.aes = target.aes,
                         lines.aes = lines.aes, inset = inset, ...)

  layer <- ggplot2::layer_sf(
    data = dummy_burst_circle(),
    mapping = mapping,
    stat = stat,
    geom = GeomSfInsetFrame,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )

  c(layer, ggplot2::coord_sf(default = TRUE))
}

frame_params <- c("source.aes", "target.aes", "lines.aes")

GeomSfInsetFrame <- ggplot2::ggproto("GeomSfInsetFrame", ggplot2::GeomSf,
  extra_params = c(ggplot2::GeomSf$extra_params, frame_params, "inset"),

  default_aes = ggplot2::aes(
    linewidth = 0.4,
    stroke = 0.4,
    colour = "gray40",
    fill = NA,
  ),

  setup_data = function(data, params) {
    if (any(data$group != -1)) {
      cli::cli_abort("{.fn geom_inset_frame} does not currently support grouped data")
    }

    ggplot2::GeomSf$setup_data(data, params)
  },

  draw_layer = function(self, data, params, layout, coord) {
    n_panels <- nlevels(as.factor(data$PANEL))
    offsets <- seq(1L, n_panels * 4L, 4L)

    # replace the dummy data with the real configured inset
    inset <- get_inset_config(params$inset, coord)
    if (is.null(inset)) {
      cli::cli_abort("Inset configuration is required for {.fn geom_inset_frame}")
    }
    frame <- make_burst_circle(inset)
    data$geometry <- rep(frame, n_panels)

    for (param in names(params$source.aes)) {
      if (!param %in% names(data)) {
        cli::cli_abort("Parameter {.arg {param}} in {.arg source.aes} does not exist in the layer data")
      }
      data[,param][offsets + 0L] <- params$source.aes[[param]]
    }

    for (param in names(params$target.aes)) {
      if (!param %in% names(data)) {
        cli::cli_abort("Parameter {.arg {param}} in {.arg target.aes} does not exist in the layer data")
      }
      data[,param][offsets + 1L] <- params$target.aes[[param]]
    }

    for (param in names(params$lines.aes)) {
      if (!param %in% names(data)) {
        cli::cli_abort("Parameter {.arg {param}} in {.arg lines.aes} does not exist in the layer data")
      }
      data[,param][c(offsets + 2L, offsets + 3L)] <- params$lines.aes[[param]]
    }

    ggplot2::GeomSf$draw_layer(data, params, layout, coord)
  },
)

# This must be the same size as the real inset so that the data frame is correctly
# handled by the ggplot machinery, but it will get replaced by the real frame
# at layout time. It also needs to have a non-NA CRS but the specific choice isn't
# important (hopefully).
dummy_burst_circle <- function () {
  sf::st_sfc(
    sf::st_polygon(),
    sf::st_polygon(),
    sf::st_linestring(),
    sf::st_linestring(),
    crs = "+proj=eqc"
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
  c(viewport, result, lines)
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

  if (hypot < short) {
    cli::cli_abort(c("Inset overlaps completely with the source part of the base map",
                     "x" = "There is no way to draw bitangents (the burst lines)",
                     "i" = "Adjust {.field translation} or {.field centre} to reduce the overlap"))
  }
  # if (hypot < short) return(sf::st_sfc()) # one circle fully inside the other
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
