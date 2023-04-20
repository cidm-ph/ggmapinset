#' Visualise sf objects with insets
#'
#' This is a wrapper around [ggplot2::geom_sf()] that assists with creating map
#' insets.
#' First, configure an inset using [configure_inset()], then pass around the
#' configuration object using [coord_sf_inset()] or the \code{inset} parameter.
#' After specifying all your usual geoms, use [geom_inset_frame()] to add a frame
#' around the inset that connects it to the main map.
#'
#' Internally this works by creating two layers: one for the base map, and one
#' for the inset. These can be separately controlled by the `map_base` and
#' `map_inset` parameters. If `inset` is not specified, this geom will instead
#' behave like [ggplot2::geom_sf()].
#'
#' When an inset is configured, the default creates both base and inset layers
#' using the same aesthetic mapping and params:
#'
#'     geom_sf_inset(...)
#'
#' You can alternatively specify the two layers separately:
#'
#'     # draw the base map only (both versions are equivalent):
#'     geom_sf(...)
#'     geom_sf_inset(..., map_inset = "none")
#'
#'     # separately, draw the inset map only:
#'     geom_sf_inset(..., map_base = "none")
#'
#' `stat_sf_inset()` works the same [ggplot2::stat_sf()] except that it also
#' expands the axis limits to account for the inset area.
#'
#' @param inset Inset configuration; see [configure_inset()].
#'   If `NA` (the default), this is inherited from the coord (see [coord_sf_inset()]).
#' @param map_base Controls the layer with the base map. Possible values are
#'   `"normal"` to create a layer as though the inset were not specified,
#'   `"clip"` to create a layer with the inset viewport cut out, and
#'   `"none"` to prevent the insertion of a layer for the base map.
#' @param map_inset Controls the layer with the inset map. Possible values are
#'   `"auto"` to choose the behaviour based on whether \code{inset} is specified,
#'   `"normal"` to create a layer with the viewport cut out and transformed, and
#'   `"none"` to prevent the insertion of a layer for the viewport map.
#' @param mapping,data,stat,geom,position,na.rm,show.legend,inherit.aes,...
#'   See [ggplot2::geom_sf()].
#'
#' @returns A ggplot layer similar to [ggplot2::geom_sf()] but transformed according to the
#'   inset configuration.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' ggplot(nc) +
#'   geom_sf_inset(aes(fill = AREA)) +
#'   geom_inset_frame() +
#'   coord_sf_inset(inset = configure_inset(
#'     centre = sf::st_sfc(sf::st_point(c(-80, 35.5)), crs = 4326),
#'     scale = 1.5, translation = c(-50, -140), radius = 50, units = "mi"))
geom_sf_inset <- function(mapping = ggplot2::aes(), data = NULL,
                          stat = "sf_inset", position = "identity",
                          ...,
                          inset = NA,
                          map_base = c("normal", "clip", "none"),
                          map_inset = c("auto", "normal", "none"),
                          na.rm = TRUE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  params = rlang::list2(na.rm = na.rm, ...)

  build_sf_inset_layers(data = data, mapping = mapping,
                        stat = stat, position = position,
                        show.legend = show.legend, inherit.aes = inherit.aes,
                        params = params, inset = inset,
                        map_base = map_base, map_inset = map_inset)
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname geom_sf_inset
GeomSfInset <- ggplot2::ggproto("GeomSfInset", ggplot2::GeomSf,
  draw_panel = function(self, data, panel_params, coord,
                        inset = NA, inset_mode = "normal", ...) {
    inset <- get_inset_config(inset, coord)
    if (!is.null(inset) && inset_mode != "none") {
      data <- switch(inset_mode,
                     normal = transform_only_viewport(data, inset),
                     cutout = remove_viewport(data, inset))
    }

    ggplot2::GeomSf$draw_panel(data, panel_params, coord, ...)
  },

  # NOTE: this is a workaround for a ggplot2 behaviour/bug
  # https://github.com/tidyverse/ggplot2/issues/1516#issuecomment-1507927792
  draw_group = function(self, data, panel_params, coord,
                        inset = NULL, inset_mode = "normal", ...) { }
)

transform_only_viewport <- function(data, inset) {
  inset <- make_inset_config(inset)
  radius <- inset_radius(inset)
  scale = inset_scale(inset)
  translation = inset_translation(inset)
  result <- with_crs_working(
    inset_crs_working(inset),
    sf::st_sf(data), inset_centre(inset),
    .f = function(data, centre) {
      geometry <- sf::st_geometry(data)
      viewport <- circular_viewport(centre, radius)
      result <- clip_to_viewport(geometry, viewport)
      geometry <- transform(result[["geometry"]], centre, scale = scale,
                            translation = translation)
      data <- data[result[["retained"]],]
      sf::st_set_geometry(data, geometry)
    })

  if (nrow(result) == 0 && nrow(data) != 0) {
    cli::cli_warn(c("None of the spatial data is inside the inset viewport",
                     "i" = "Check your inset configuration to ensure the centre, radius, and units are correct"))
  }

  result
}

remove_viewport <- function(data, inset) {
  inset <- make_inset_config(inset)
  radius <- inset_radius(inset)
  result <- with_crs_working(
    inset_crs_working(inset),
    sf::st_sf(data), inset_centre(inset),
    .f = function(data, centre) {
      geometry <- sf::st_geometry(data)
      viewport <- circular_viewport(centre, radius)
      result <- clip_away_viewport(geometry, viewport)
      data <- data[result[["retained"]],]
      sf::st_set_geometry(data, result[["geometry"]])
    })

  if (nrow(result) == 0 && nrow(data) != 0) {
    cli::cli_warn(c("None of the spatial data is outside the inset viewport",
                     "i" = "Check your inset configuration to ensure the centre, radius, and units are correct"))
  }

  result
}
