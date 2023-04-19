#' Specify an inset configuration for the whole plot
#'
#' This allows a default inset configuration to be provided to avoid having to
#' repeat it for each layer. Any layer that is inset-aware can use this as the
#' default configuration if none is specifically provided to that layer.
#'
#' @param inset Inset configuration; see [configure_inset()].
#' @param ... Arguments passed to [ggplot2::coord_sf()]
#'
#' @export
coord_sf_inset <- function(inset, ...) {
  inset <- make_inset_config(inset)

  parent <- ggplot2::coord_sf(...)
  ggproto("CoordSfInset", parent, inset = inset)
}
