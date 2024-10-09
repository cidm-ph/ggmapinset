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
    data = dummy_frame(),
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
    offsets <- seq(1L, n_panels * 3L, 3L)

    # replace the dummy data with the real configured inset
    inset <- get_inset_config(params$inset, coord)
    if (is.null(inset)) {
      cli::cli_abort("Inset configuration is required for {.fn geom_inset_frame}")
    }
    frame <- make_frame(inset)
    data$geometry <- rep(frame, n_panels)

    for (param in names(params$source.aes)) {
      if (!param %in% names(data)) {
        cli::cli_abort("Parameter {.arg {param}} in {.arg source.aes} does not exist in the layer data")
      }
      data[, param][offsets + 0L] <- params$source.aes[[param]]
    }

    for (param in names(params$target.aes)) {
      if (!param %in% names(data)) {
        cli::cli_abort("Parameter {.arg {param}} in {.arg target.aes} does not exist in the layer data")
      }
      data[, param][offsets + 1L] <- params$target.aes[[param]]
    }

    for (param in names(params$lines.aes)) {
      if (!param %in% names(data)) {
        cli::cli_abort("Parameter {.arg {param}} in {.arg lines.aes} does not exist in the layer data")
      }
      data[, param][offsets + 2L] <- params$lines.aes[[param]]
    }

    ggplot2::GeomSf$draw_layer(data, params, layout, coord)
  },
)

# This must be the same size as the real inset so that the data frame is correctly
# handled by the ggplot machinery, but it will get replaced by the real frame
# at layout time. It also needs to have a non-NA CRS but the specific choice isn't
# important (hopefully).
dummy_frame <- function() {
  sf::st_sfc(
    sf::st_polygon(),
    sf::st_polygon(),
    sf::st_multilinestring(),
    crs = "+proj=eqc"
  )
}

make_frame <- function(inset) {
  UseMethod("make_frame")
}
