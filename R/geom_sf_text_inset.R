#' @export
#' @rdname stat_sf_coordinates_inset
#' @param stat,parse,check_overlap See [ggplot2::geom_sf_text()].
#' @inheritParams ggplot2::geom_text
geom_sf_text_inset <- function(
  mapping = aes(), data = NULL,
  stat = "sf_coordinates_inset",
  position = "identity",
  ...,
  where = "inset",
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  fun.geometry = NULL
) {
  where <- rlang::arg_match0(where, c("inset", "base"))
  if (where == "inset" && !any(c("x", "y") %in% names(mapping))) {
    mapping[["x"]] <- quote(after_stat(x_inset))
    mapping[["y"]] <- quote(after_stat(y_inset))
  }

  ggplot2::layer_sf(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
  )
}

#' @export
#' @rdname stat_sf_coordinates_inset
#' @param stat,parse,check_overlap See [ggplot2::geom_sf_text()].
#' @inheritParams ggplot2::geom_label
geom_sf_label_inset <- function(
  mapping = aes(), data = NULL,
  stat = "sf_coordinates_inset",
  position = "identity",
  ...,
  where = "inset",
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  fun.geometry = NULL
) {
  where <- rlang::arg_match0(where, c("inset", "base"))
  if (where == "inset" && !any(c("x", "y") %in% names(mapping))) {
    mapping[["x"]] <- quote(after_stat(x_inset))
    mapping[["y"]] <- quote(after_stat(y_inset))
  }

  ggplot2::layer_sf(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      parse = parse,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
  )
}
