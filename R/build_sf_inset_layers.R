#' Build layers to implement an inset-compatible geometry
#'
#' For plotting, use [geom_sf_inset()] instead. This helper is intended to be used when
#' implementing custom geometries based on [geom_sf_inset()] so that they can provide
#' parameters to control the inset.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams geom_sf_inset
#'
#' @returns A \code{ggplot} layer, or a pair of layers.
#' @export
#'
#' @examples
#' my_custom_geom <- function(mapping = ggplot2::aes(),
#'                            data = NULL,
#'                            stat = "my_custom_stat",
#'                            position = "identity",
#'                            ...,
#'                            inset = NULL,
#'                            map_base = "normal",
#'                            map_inset = "auto",
#'                            na.rm = TRUE,
#'                            inherit.aes = TRUE) {
#'   params <- rlang::list2(na.rm = na.rm, ...)
#'   build_sf_inset_layers(data = data, mapping = mapping,
#'                         stat = stat, position = position,
#'                         show.legend = show.legend,
#'                         inherit.aes = inherit.aes,
#'                         params = params,
#'                         inset = inset,
#'                         map_base = map_base,
#'                         map_inset = map_inset)
#' }
build_sf_inset_layers <- function (data, mapping, stat, position, show.legend,
                                   inherit.aes, params, inset,
                                   map_base, map_inset) {
  has_inset_cfg <- !is.null(inset) #| !is.null(mapping[["inset"]])

  if (map_inset == "auto") {
    draw_inset <- map_inset == "normal" || (map_inset == "auto" && has_inset_cfg)
    map_inset <- ifelse(draw_inset, "normal", "none")
  }
  if (map_base == "none" && map_inset == "none") {
    cli::cli_abort("{.arg map_base} and {.arg map_inset} can't both be disabled")
  }

  make_layer <- function (inset_mode) {
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
  c(base_layer, inset_layer)
}
