#' Build layers to implement an inset-compatible geometry
#'
#' For plotting, use [geom_sf_inset()] instead. These helpers are intended to be used when
#' implementing custom geometries based on [geom_sf_inset()] so that they can provide
#' parameters to control the inset.
#'
#' `build_sf_inset_layers()` should be called from a geom constructor instead of
#' `ggplot2::layer_sf()`. This allows an `inset` parameter to control the creation of two
#' layers (base + inset) as needed.
#'
#' `get_inset_config()` should always be called early inside the draw or compute function,
#' e.g. `Geom$draw_panel()` or `Stat$compute_group()` whenever that function accepts an
#' `inset` param.
#' The helper validates the inset configuration after applying fallback to the coord's
#' inset configuration if needed.
#'
#' @param data,mapping,stat,position,show.legend,inherit.aes,params  See [ggplot2::layer()].
#' @inheritParams geom_sf_inset
#' @param coord Coord object for the plot.
#'
#' @returns
#'   - For `build_sf_inset_layers()`: a \code{ggplot} layer, or a pair of layers.
#'   - For `get_inset_config()`: a valid inset config object or `NULL`.
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
#'   inset = ggplot2::waiver(),
#'   map_base = "normal",
#'   map_inset = "auto",
#'   na.rm = TRUE,
#'   inherit.aes = TRUE
#' ) {
#'   params <- rlang::list2(na.rm = na.rm, ...)
#'   build_sf_inset_layers(
#'     data = data,
#'     mapping = mapping,
#'     stat = stat,
#'     position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = params,
#'     inset = inset,
#'     map_base = map_base,
#'     map_inset = map_inset
#'   )
#' }
#'
#' # defining a new geom deriving from geom_sf()
#' GeomCustom <- ggplot2::ggproto("GeomCustom", ggplot2::GeomSf,
#'   draw_panel = function(self, data, panel_params, coord, inset = ggplot2::waiver()) {
#'     inset <- get_inset_config(inset, coord)
#'
#'     # do something with the inset ...
#'
#'     # note that this example doesn't pass on the remaining geom_sf params but
#'     # in real usage you would probably want to do that
#'     ggplot2::ggproto_parent(ggplot2::GeomSf, self)$draw_panel(data, panel_params, coord)
#'   },
#' )
build_sf_inset_layers <- function(
  data,
  mapping,
  stat,
  position,
  show.legend,
  inherit.aes,
  params,
  inset,
  map_base = "normal",
  map_inset = "auto"
) {
  if (!is.null(inset) && length(inset) == 1L && is.na(inset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "build_sf_inset_layers(inset = 'must not be NA')",
      details = "use `inset = waiver()` instead to default to the coord's inset"
    )
    inset <- waiver()
  }
  has_inset_cfg <- !is.null(inset)

  map_base <- rlang::arg_match0(map_base, c("normal", "clip", "none"))
  map_inset <- rlang::arg_match0(map_inset, c("auto", "normal", "none"))

  if (map_inset == "auto") {
    draw_inset <- map_inset == "normal" ||
      (map_inset == "auto" && has_inset_cfg)
    map_inset <- if (draw_inset) "normal" else "none"
  }
  if (map_base == "none" && map_inset == "none") {
    cli::cli_abort(
      "{.arg map_base} and {.arg map_inset} can't both be disabled"
    )
  }

  make_layer <- function(inset_mode, layer_type) {
    ggplot2::layer_sf(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomSfInset,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = modifyList(
        params,
        list(inset = inset, inset_mode = inset_mode, layer_type = layer_type)
      )
    )
  }

  # There are 3 possibilities for each of the two layers:
  #   1) we know it needs to be drawn (map_base/map_inset not "none"), or
  #   2) we know it should not be drawn (map_base/map_inset is "none"), or
  #   3) we don't know either way because the inset configuration is deferred
  #      to the coord and will be computed later.
  # For 2) we can just set the layer to NULL to avoid adding an extra layer to
  # the plot. For 3) we have to add the layer but allow for it to be a no-op.
  base_layer <- switch(
    map_base,
    normal = make_layer("none", "base"),
    clip = make_layer("cutout", "base"),
    none = NULL
  )
  inset_layer <- switch(
    map_inset,
    normal = make_layer("normal", "inset"),
    none = NULL
  )
  c(base_layer, inset_layer, ggplot2::coord_sf(default = TRUE))
}

#' @export
#' @rdname build_sf_inset_layers
get_inset_config <- function(inset, coord) {
  if (is.null(inset)) {
    return(make_inset_config(NULL))
  }

  if (!rlang::is_empty(inset) && is.na(inset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "get_inset_config(inset = 'must not be NA')",
      details = "use `inset = waiver()` instead to default to the coord's inset"
    )
    inset <- waiver()
  }

  if (is_waiver(inset)) {
    if (inherits(coord, "CoordSfInset")) {
      make_inset_config(coord$inset)
    } else {
      make_inset_config(NULL)
    }
  } else {
    make_inset_config(inset)
  }
}
