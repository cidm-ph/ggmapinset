#' Build layers to implement an inset-compatible geometry
#'
#' For plotting, use [geom_sf_inset()] instead. This helper is intended to be used when
#' implementing custom geometries based on [geom_sf_inset()] so that they can provide
#' parameters to control the inset.
#'
#' @param data,mapping,stat,position,show.legend,inherit.aes,params  See [ggplot2::layer()].
#' @inheritParams geom_sf_inset
#'
#' @returns A \code{ggplot} layer, or a pair of layers.
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' my_custom_geom <- function(
#'   mapping = ggplot2::aes(),
#'   data = NULL,
#'   stat = "my_custom_stat",
#'   position = "identity",
#'   ...,
#'   inset = NA,
#'   map_base = "normal",
#'   map_inset = "auto",
#'   na.rm = TRUE,
#'   inherit.aes = TRUE
#' ) {
#'   params <- rlang::list2(na.rm = na.rm, ...)
#'   build_sf_inset_layers(
#'     data = data, mapping = mapping,
#'     stat = stat, position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = params,
#'     inset = inset,
#'     map_base = map_base,
#'     map_inset = map_inset
#'   )
#' }
build_sf_inset_layers <- function(
  data, mapping, stat, position, show.legend, inherit.aes, params, inset,
  map_base = "normal", map_inset = "auto"
) {
  has_inset_cfg <- !is.null(inset)

  map_base <- rlang::arg_match0(map_base, c("normal", "clip", "none"))
  map_inset <- rlang::arg_match0(map_inset, c("auto", "normal", "none"))

  if (map_inset == "auto") {
    draw_inset <- map_inset == "normal" || (map_inset == "auto" && has_inset_cfg)
    map_inset <- if (draw_inset) "normal" else "none"
  }
  if (map_base == "none" && map_inset == "none") {
    cli::cli_abort("{.arg map_base} and {.arg map_inset} can't both be disabled")
  }

  make_layer <- function(inset_mode) {
    ggplot2::layer_sf(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomSfInset,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = modifyList(params, list(inset = inset, inset_mode = inset_mode))
    )
  }

  base_layer <- switch(map_base,
                       normal = make_layer("none"),
                       clip = make_layer("cutout"),
                       none = NULL)
  inset_layer <- switch(map_inset,
                        normal = make_layer("normal"),
                        none = NULL)
  c(base_layer, inset_layer, ggplot2::coord_sf(default = TRUE))
}

#' Get the inset configuration from the params or coord
#'
#' This is a helper for implementing inset-aware ggplot layers. If the `inset` is
#' missing (`NA`) then the default inset configuration is retrieved from the coord.
#'
#' @param inset Inset passed in as a param to the layer
#' @param coord Coord object for the plot
#'
#' @returns Inset configuration or `NULL`
#' @export
#' @examples
#' # defining a new geom deriving from geom_sf()
#' GeomCustom <- ggplot2::ggproto("GeomCustom", ggplot2::GeomSf,
#'   draw_panel = function(self, data, panel_params, coord, inset = NA) {
#'     inset <- get_inset_config(inset, coord)
#'
#'     # do something with the inset ...
#'
#'     # note that this example doesn't pass on the remaining geom_sf params but
#'     # in real usage you would probably want to do that
#'     ggplot2::ggproto_parent(ggplot2::GeomSf, self)$draw_panel(data, panel_params, coord)
#'   },
#' )
get_inset_config <- function(inset, coord) {
  if (is.null(inset)) {
    NULL
  } else if (is.na(inset)) {
    if (inherits(coord, "CoordSfInset")) {
      make_inset_config(coord$inset)
    } else {
      NULL
    }
  } else {
    make_inset_config(inset)
  }
}
