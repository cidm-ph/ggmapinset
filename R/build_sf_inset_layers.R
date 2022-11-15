#' Build layers to implement an inset-compatible geometry
#'
#' For plotting, use [geom_sf_inset()] insetad. This helper is intended to help
#' with implementing geometries based on \code{geom_sf_inset()}.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams geom_sf_inset
#'
#' @return A \code{ggplot} layer, or a pair of layers.
#' @export
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
