#' Add a frame and burst lines for an inset
#'
#' The frame is computed from the inset configuration, so any \code{data} passed
#' to this layer is ignored. The frame is an sf object consisting of three features:
#' the source area, the target area (a scaled and translated version of the source
#' area), and the connecting/burst lines.
#'
#' Burst lines for circular insets are bitangenets (tangent to both the source and
#' target circles) or absent if the circles are nested.
#' Burst lines for rectangular insets are the shortest line from each corner of the
#' source rectangle to any corner of the target rectangle, after excluding any such
#' lines that intersect either rectangle or each other.
#' When the burst lines are absent due to geometrical constraints, there will still
#' be a corresponding (empty) feature in the frame layer's data.
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
#'   geom_inset_frame(
#'     source.aes = list(fill = "red", alpha = 0.2, linewidth = 0),
#'     target.aes = list(colour = "blue"),
#'     lines.aes = list(linetype = 2, linewidth = 2)
#'   ) +
#'   coord_sf_inset(inset = configure_inset(
#'     shape_circle(
#'       centre = sf::st_sfc(sf::st_point(c(-82, 35)), crs = 4326),
#'       radius = 50
#'     ),
#'     scale = 5, translation = c(0, -260), units = "mi"
#'   ))
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

get_sf_aes_default <- function(aes_name) {
  line_def <- ggplot2::GeomLine$default_aes[[aes_name]]
  polygon_def <- ggplot2::GeomPolygon$default_aes[[aes_name]]
  if (aes_name == "fill") polygon_def <- NA_character_
  else if (aes_name == "colour") return(rep("grey40", 3))
  else if (aes_name == "linewidth") return(rep(0.2, 3))

  values <- c(polygon_def, polygon_def, line_def)
  if (aes_name == "alpha") values <- as.numeric(values)

  values
}


#' @importFrom vctrs vec_slice
GeomSfInsetFrame <- ggplot2::ggproto("GeomSfInsetFrame", ggplot2::GeomSf,
  extra_params = c(ggplot2::GeomSf$extra_params, frame_params, "inset"),

  default_aes = modifyList(
    ggplot2::GeomSf$default_aes,
    list(
      fill = NA_character_,
      colour = "grey40",
      linewidth = 0.2,
      alpha = NA_real_
    )
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

    extra_cols <- setdiff(
      c(names(params$source.aes), names(params$target.aes), names(params$lines.aes)),
      names(data)
    )
    for (param in extra_cols) {
      data[, param] <- rep(get_sf_aes_default(param), n_panels)
    }

    for (param in names(params$source.aes)) {
      vctrs::vec_slice(data[, param], offsets + 0L) <- params$source.aes[[param]]
    }

    for (param in names(params$target.aes)) {
      vctrs::vec_slice(data[, param], offsets + 1L) <- params$target.aes[[param]]
    }

    for (param in names(params$lines.aes)) {
      vctrs::vec_slice(data[, param], offsets + 2L) <- params$lines.aes[[param]]
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
