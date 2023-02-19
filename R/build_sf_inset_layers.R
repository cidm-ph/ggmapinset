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
#'                            inset_copy = TRUE,
#'                            inset_clip = TRUE,
#'                            na.rm = TRUE,
#'                            inherit.aes = TRUE) {
#'   params <- rlang::list2(na.rm = na.rm, ...)
#'   build_sf_inset_layers(data = data, mapping = mapping,
#'                         stat = stat, position = position,
#'                         show.legend = show.legend,
#'                         inherit.aes = inherit.aes,
#'                         params = params,
#'                         inset = inset,
#'                         inset_copy = inset_copy,
#'                         inset_clip = inset_clip)
#' }
build_sf_inset_layers <- function (data, mapping, stat, position, show.legend,
                                   inherit.aes, params, inset,
                                   inset_copy, inset_clip) {
  has_inset_cfg <- !is.null(inset) | !is.null(mapping[["inset"]])

  make_layer <- function (is_inset) {
    ggplot2::layer_sf(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomSfInset,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = modifyList(params, list(inset = inset,
                                       inset_enable = is_inset,
                                       inset_invert = has_inset_cfg & inset_clip))
    )
  }

  layers <- c()
  if (inset_copy | !has_inset_cfg) layers <- c(layers, make_layer(FALSE))
  if (              has_inset_cfg) layers <- c(layers, make_layer(TRUE))
  layers
}
