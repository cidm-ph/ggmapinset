#' Extract coordinates from 'sf' objects (inset-aware)
#'
#' Reduce spatial data to coordinates in the same way as [`stat_sf_coordinates()`][ggplot2::stat_sf_coordinates].
#' The result can then be used by [`geom_sf()`][ggplot2::geom_sf] or
#' [`geom_sf_inset()`][ggmapinset::geom_sf_inset] or any geom that needs `x` and
#' `y` aesthetics.
#'
#' @section Required aesthetics:
#' \describe{
#'   \item{geometry}{The sf geometry column containing spatial features}
#' }
#'
#' @section Computed variables:
#' \describe{
#'   \item{x}{X dimension of the simple feature}
#'   \item{y}{Y dimension of the simple feature}
#'   \item{x_inset}{X dimension of the simple feature after inset transformation}
#'   \item{y_inset}{Y dimension of the simple feature after inset transformation}
#'   \item{inside_inset}{logical indicating points inside the inset viewport}
#'   \item{inset_scale}{1 for points outside the inset, otherwise the configured inset scale parameter}
#' }
#'
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,... See [ggplot2::stat_sf_coordinates()].
#' @inheritParams ggplot2::stat_sf_coordinates
#' @inheritParams geom_sf_inset
#'
#' @export
#' @returns A plot layer
#' @examples
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' ggplot(nc) +
#'   geom_sf_inset() +
#'   geom_inset_frame() +
#'   geom_sf_text(aes(x = after_stat(x_inset), y = after_stat(y_inset), label = NAME),
#'     stat = "sf_coordinates_inset") +
#'   coord_sf_inset(inset = configure_inset(
#'     centre = sf::st_sfc(sf::st_point(c(-80, 35.5)), crs = 4326),
#'     scale = 1.5, translation = c(-50, -140), radius = 50, units = "mi"))
stat_sf_coordinates_inset <- function(mapping = ggplot2::aes(), data = NULL,
                                      geom = "sf_inset", position = "identity",
                                      ...,
                                      inset = NA,
                                      fun.geometry = NULL,
                                      na.rm = TRUE,
                                      show.legend = NA,
                                      inherit.aes = TRUE) {
  ggplot2::layer_sf(
    mapping = mapping,
    data = data,
    stat = StatSfCoordinatesInset,
    geom = geom,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = rlang::list2(
      inset = inset,
      fun.geometry = fun.geometry,
      na.rm = na.rm,
      ...
    ))
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_sf_coordinates_inset
StatSfCoordinatesInset <- ggplot2::ggproto("StatSfCoordinatesInset", ggplot2::StatSfCoordinates,
  compute_group = function(data, scales, coord, fun.geometry = NULL, inset = NA) {
    inset <- get_inset_config(inset, coord)

    if (is.null(inset)) {
      data <- ggplot2::StatSfCoordinates$compute_panel(data, scales, coord, fun.geometry)
    } else {
      # with a configured working CRS, we can compute fun.geometry in that CRS
      crs_orig <- sf::st_crs(data$geometry)
      crs_working <- inset_crs_working(inset)
      geom_orig <- data$geometry
      data$geometry <- sf::st_transform(data$geometry, crs_working)
      data <- ggplot2::StatSfCoordinates$compute_panel(data, scales, coord, fun.geometry)
      data$geometry <- geom_orig

      # cast the x and y cols into an sfc of points
      geom <- sf::st_multipoint(matrix(c(data$x, data$y), ncol = 2))
      geom <- sf::st_sfc(geom, crs = crs_working)
      geom <- sf::st_cast(geom, "POINT")

      coordinates <- sf::st_coordinates(sf::st_transform(geom, crs_orig))
      data$x <- coordinates[, "X"]
      data$y <- coordinates[, "Y"]
    }

    # we can compute where points end up after the inset transformation in case
    # this is relevant for downstream users
    data$x_inset <- data$x
    data$y_inset <- data$y
    data$inside_inset <- NA
    data$inset_scale <- 1
    if (!is.null(inset)) {
      # cut out and transform the inset viewport from these points
      centre <- sf::st_transform(inset_centre(inset), crs_working)
      scale <- inset_scale(inset)
      viewport <- circular_viewport(centre, inset_radius(inset))
      result <- clip_to_viewport(geom, viewport)
      geometry <- transform(result[["geometry"]], centre,
                            scale = scale,
                            translation = inset_translation(inset))
      geometry <- sf::st_transform(geometry, crs_orig)

      data$inside_inset <- FALSE
      data$inside_inset[result[["retained"]]] <- TRUE
      data$inset_scale[data$inside_inset] <- scale

      coordinates <- sf::st_coordinates(geometry)
      data$x_inset[data$inside_inset] <- coordinates[, "X"]
      data$y_inset[data$inside_inset] <- coordinates[, "Y"]
    }

    # we also need to let the extend the coord boundaries and range to include
    # the transformed inset
    if (!is.null(inset) && inherits(coord, "CoordSf")) {
      if (sf::st_crs(inset) != sf::st_crs(data)) {
        cli::cli_warn(c("Inset coordinate reference system does not match data",
                        "i" = "The {.field centre} of the inset uses a different CRS to the data; the inset might be drawn incorrectly"))
      }

      bbox <- inset_bbox(inset)

      coord$record_bbox(
        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
      )
    }

    data
  }
)
