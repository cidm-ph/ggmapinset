#' Visualise sf objects with insets
#'
#' This is a wrapper around [ggplot2::geom_sf()] that assists with creating map
#' insets.
#'
#' First, configure an inset using [configure_inset()], then pass the
#' configuration object to each applicable layer using the \code{inset}
#' parameter.
#'
#' After specifying all your usual geoms, use [geom_inset_frame()] to add a frame
#' around the inset that connects it to the main map.
#'
#' Internally this works by inserting duplicates of the \code{geom_sf()} layers
#' where the duplicates have been transformed and cropped to fit into the inset.
#' The way that this copying works can be controlled with the \code{inset_copy}
#' parameter if for example you'd like a different aesthetic mapping for the
#' copy that goes in the inset. The default copies the layers:
#'
#'     geom_sf_inset(aes(...), inset = inset_cfg, ...)
#'
#' but this version specifies them separately:
#'
#'     # aesthetics for main map only:
#'     geom_sf(aes(...), ...)
#'     # aesthetics for inset map only:
#'     geom_sf_inset(aes(...), inset = inset_cfg, inset_copy = FALSE, ...)
#'
#' @param inset Inset configuration; see [configure_inset()].
#' @param inset_copy Draw both the base layers and the inset layers using the same
#'   configuration. Only relevant when \code{inset} is specified.
#' @param inset_clip When an inset is drawn, place included points only in the inset.
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... See [ggplot2::geom_sf()]
#' @export
geom_sf_inset <- function(mapping = ggplot2::aes(), data = NULL,
                          stat = "sf", position = "identity",
                          ...,
                          inset = NULL,
                          inset_copy = TRUE,
                          inset_clip = FALSE,
                          na.rm = TRUE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  params = rlang::list2(na.rm = na.rm, ...)

  build_sf_inset_layers(data = data, mapping = mapping,
                        stat = stat, position = position,
                        show.legend = show.legend, inherit.aes = inherit.aes,
                        params = params, inset = inset,
                        inset_copy = inset_copy, inset_clip = inset_clip)
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname geom_sf_inset
#' @importFrom utils modifyList
GeomSfInset <- ggplot2::ggproto("GeomSfInset", ggplot2::GeomSf,
  default_aes = modifyList(ggplot2::GeomSf$default_aes,
                           list(inset = NULL, inset_invert = FALSE, inset_enable = TRUE),
                           keep.null = TRUE),

  draw_panel = function(self, data, ...) {
    if (!is.null(data[["inset"]])) {
      inset <- make_inset_config(data$inset[[1]])
      geom <- data$geometry
      if (!inherits(geom, "sfc")) geom <- sf::st_geometry(geom)

      if (data$inset_enable[[1]]) {
        inset_geom <- crop_inset_circle(geom,
                                        centre = inset_centre(inset),
                                        radius = inset_radius(inset),
                                        scale = inset_scale(inset),
                                        translation = inset_translation(inset),
                                        crs_working = inset_crs_working(inset))
        data <- data[attr(inset_geom, "retained"),]
        data$geometry <- inset_geom
      } else if (data$inset_invert[[1]]) {
        inset_geom <- clip_inset_circle(geom,
                                        centre = inset_centre(inset),
                                        radius = inset_radius(inset),
                                        crs_working = inset_crs_working(inset))
        data <- data[attr(inset_geom, "retained"),]
        data$geometry <- inset_geom
      }
    }

    ggplot2::ggproto_parent(ggplot2::GeomSf, self)$draw_panel(data, ...)
  }
)

crop_inset_circle <- function(x, centre, radius, scale, translation,
                              crs_working) {
  crs_orig <- sf::st_crs(x)

  centre <- sf::st_transform(centre, crs_working)
  viewport <- sf::st_buffer(centre, radius)
  x <- sf::st_transform(x, crs_working)
  result <- sf::st_intersection(x, viewport)
  retained <- attr(result, "idx")[,1]

  if (!is.null(scale)) {
    result <- (result - centre) * scale + centre
    result <- sf::st_set_crs(result, crs_working)
  }
  if (!is.null(translation)) {
    result <- sf::st_set_crs(result + translation, crs_working)
  }

  result <- sf::st_transform(result, crs_orig)
  attr(result, "retained") <- retained
  result
}

clip_inset_circle <- function(x, centre, radius, crs_working) {
  crs_orig <- sf::st_crs(x)

  centre <- sf::st_transform(centre, crs_working)
  viewport <- sf::st_buffer(centre, radius)
  x <- sf::st_transform(x, crs_working)
  result <- sf::st_difference(x, viewport)
  retained <- attr(result, "idx")[,1]
  result <- sf::st_transform(result, crs_orig)
  attr(result, "retained") <- retained
  result
}
