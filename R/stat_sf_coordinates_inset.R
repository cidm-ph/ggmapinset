#' Extract coordinates from 'sf' objects (inset-aware)
#'
#' Reduce spatial data to coordinates in the same way as [`stat_sf_coordinates()`][ggplot2::stat_sf_coordinates].
#' The result can then be used by [`geom_sf()`][ggplot2::geom_sf] or
#' [`geom_sf_inset()`][ggmapinset::geom_sf_inset] or any geom that needs `x` and
#' `y` aesthetics. As this is particularly useful for text, wrappers are
#' provided for [`geom_text()`][ggplot2::geom_text] and [`geom_label()`][ggplot2::geom_label].
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
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,fun.geometry,... See [ggplot2::stat_sf_coordinates()].
#' @param where Specifies how the text position interacts with the inset.
#'   `"inset"` means that any points in the inset area are drawn on the inset map,
#'   `"base"` puts them on the base map. This setting is merely a shorthand for
#'   setting the position aesthetics to `after_stat(x_inset)` or `after_stat(x)`
#'   respectively, so will have no effect if these are specified in the mapping.
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
#'   geom_sf_text(
#'     aes(x = after_stat(x_inset), y = after_stat(y_inset), label = NAME),
#'     stat = "sf_coordinates_inset") +
#'   coord_sf_inset(configure_inset(
#'     shape_circle(
#'       centre = sf::st_sfc(sf::st_point(c(-80, 35.5)), crs = 4326),
#'       radius = 50
#'     ),
#'     scale = 1.5, translation = c(-50, -140), units = "mi"
#'   ))
stat_sf_coordinates_inset <- function(
  mapping = ggplot2::aes(), data = NULL,
  geom = "point", position = "identity",
  ...,
  inset = NA,
  fun.geometry = NULL,
  where = "inset",
  precision = NA,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  where <- rlang::arg_match0(where, c("inset", "base"))
  if (where == "inset" && !any(c("x", "y") %in% names(mapping))) {
    mapping[["x"]] <- quote(after_stat(x_inset))
    mapping[["y"]] <- quote(after_stat(y_inset))
  }

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
      precision = precision,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_sf_coordinates_inset
StatSfCoordinatesInset <- ggplot2::ggproto("StatSfCoordinatesInset", ggplot2::StatSfCoordinates,
  compute_group = function(data, scales, coord, fun.geometry = NULL, inset = NA, crs_working = NULL, precision = NA) {
    inset <- get_inset_config(inset, coord)

    if (is.null(crs_working)) {
      if (!is.null(inset)) crs_working <- inset_crs_working(inset)
      else crs_working <- sf::NA_crs_
    }

    if (is.null(fun.geometry)) {
      fun.geometry <- function(x) sf::st_point_on_surface(sf::st_zm(x))
    }

    crs_orig <- sf::st_crs(data$geometry)
    points_sfc <- if (is.na(crs_working)) {
      fun.geometry(data$geometry)
    } else {
      with_crs_working(crs_working, data$geometry, .f = fun.geometry)
    }

    coordinates <- sf::st_coordinates(points_sfc)
    data$x <- coordinates[, "X"]
    data$y <- coordinates[, "Y"]

    # we can compute where points end up after the inset transformation in case
    # this is relevant for downstream users
    data$x_inset <- data$x
    data$y_inset <- data$y
    data$inside_inset <- NA
    data$inset_scale <- 1
    if (!is.null(inset)) {
      data$inside_inset <- FALSE
      crs_working2 <- inset_crs_working(inset)
      # cut out and transform the inset viewport from these points
      centre <- sf::st_transform(inset_centre(inset), crs_working2)
      scale <- inset_scale(inset)
      geometry <- sf::st_transform(points_sfc, crs_working2)
      result <- clip_to_viewport(geometry, inset_viewport(inset))
      if (length(result[["retained"]]) > 0) {
        geometry <- transform(result[["geometry"]], centre,
                              scale = scale,
                              translation = inset_translation(inset),
                              precision = precision)
        geometry <- sf::st_transform(geometry, crs_orig)

        data$inside_inset[result[["retained"]]] <- TRUE
        data$inset_scale[data$inside_inset] <- scale

        coordinates <- sf::st_coordinates(geometry)
        data$x_inset[data$inside_inset] <- coordinates[, "X"]
        data$y_inset[data$inside_inset] <- coordinates[, "Y"]
      }
    }

    # we also need to let the extend the coord boundaries and range to include
    # the transformed inset
    if (inherits(coord, "CoordSf")) {
      bbox <- sf::st_bbox(points_sfc)
      coord$record_bbox(
        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
      )

      if (!is.null(inset)) {
        # if (sf::st_crs(inset) != sf::st_crs(data)) {
        #   cli::cli_warn(c("Inset coordinate reference system does not match data",
        #                   "i" = "The {.field centre} of the inset uses a different CRS to the data; the inset might be drawn incorrectly"))
        # }

        bbox <- inset_bbox(inset)
        coord$record_bbox(
          xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
          ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
        )
      }
    }

    data
  }
)
