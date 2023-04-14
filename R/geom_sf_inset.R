#' Visualise sf objects with insets
#'
#' This is a wrapper around [ggplot2::geom_sf()] that assists with creating map
#' insets. You'll normally want to pair it with [geom_inset_frame()].
#'
#' First, configure an inset using [configure_inset()], then pass the
#' configuration object to each applicable layer using the \code{inset}
#' parameter.
#'
#' After specifying all your usual geoms, use [geom_inset_frame()] to add a frame
#' around the inset that connects it to the main map.
#'
#' Internally this works by inserting a duplicate of the base \code{geom_sf()}
#' layer which has been transformed and cropped to fit into the inset frame.
#' Using \code{inset_copy = FALSE} will suppress the base layer and only draw the
#' inset layer, allowing the base layer to be specified separately with different
#' aesthetic mapping and parameters. The default copies the layer:
#'
#'     geom_sf_inset(aes(...), inset = inset_cfg, ...)
#'
#' but this version specifies the two layers separately:
#'
#'     # aesthetics for base map only:
#'     geom_sf(aes(...), ...)
#'     # aesthetics for inset map only:
#'     geom_sf_inset(aes(...), inset = inset_cfg, inset_copy = FALSE, ...)
#'
#' @param inset Inset configuration; see [configure_inset()].
#' @param inset_copy Draw both the base layer and the inset layer using the same
#'   configuration. Only relevant when \code{inset} is specified.
#' @param inset_clip Clip away the part of the base layer corresponding to the
#'   inset frame. Only relevant when \code{inset} is specified.
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,...
#'   See [ggplot2::geom_sf()]
#'
#' @returns A ggplot layer similar to [ggplot2::geom_sf()] but transformed according to the
#'   inset configuration.
#' @export
#'
#' @examples
#' library(sf)
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' cfg <- configure_inset(
#'   centre = st_sfc(st_point(c(-82, 35)), crs = 4326),
#'   scale = 2,
#'   translation = c(10, -100),
#'   radius = 50,
#'   units = "mi")
#'
#' ggplot(nc) + geom_sf_inset(aes(fill = AREA), inset = cfg) + coord_sf()
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
        data <- transform_only_viewport(data, inset)
      } else if (data$inset_invert[[1]]) {
        data <- remove_viewport(data, inset)
      }
    }

    ggplot2::ggproto_parent(ggplot2::GeomSf, self)$draw_panel(data, ...)
  }
)

transform_only_viewport <- function(data, inset) {
  radius <- inset_radius(inset)
  scale = inset_scale(inset)
  translation = inset_translation(inset)
  result <- with_crs_working(
    inset_crs_working(inset),
    sf::st_sf(data), inset_centre(inset),
    .f = function(data, centre) {
      geometry <- sf::st_geometry(data)
      result <- clip_to_circular_viewport(geometry, centre, radius)
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
  radius <- inset_radius(inset)
  result <- with_crs_working(
    inset_crs_working(inset),
    sf::st_sf(data), inset_centre(inset),
    .f = function(data, centre) {
      geometry <- sf::st_geometry(data)
      result <- clip_away_circular_viewport(geometry, centre, radius)
      data <- data[result[["retained"]],]
      sf::st_set_geometry(data, result[["geometry"]])
    })

  if (nrow(result) == 0 && nrow(data) != 0) {
    cli::cli_warn(c("None of the spatial data is outside the inset viewport",
                     "i" = "Check your inset configuration to ensure the centre, radius, and units are correct"))
  }

  result
}
