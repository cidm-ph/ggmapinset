#' Convert an object to a point in a suitable format
#'
#' This is an internal helper used by the shape definition functions
#' to process their `centre` parameters, where present.
#' It is not intended to be used directly.
#' Instead, it allows extensions to integrate with the shape
#' functions so that they can accept new definitions.
#'
#' @param centre Object defining the centre point.
#' @param ... Unused in the default implementation.
#' @returns A `sfc` object containins a single [`sf::st_point`] feature
#'   and with an appropriate CRS set.
#' @export
coerce_centre <- function(centre, ...) {
  UseMethod("coerce_centre")
}

#' @export
coerce_centre.default <- function(centre, ...) {
  rlang::check_dots_empty()

  if (is.null(centre)) {
    cli::cli_abort("Shape {.arg centre} must be provided")
  }
  crs_input <- sf::NA_crs_
  if (inherits(centre, "sf")) {
    centre <- sf::st_geometry(centre)
  }
  if (inherits(centre, "sfc")) {
    crs_input <- sf::st_crs(centre)
    if (length(centre) == 0) {
      cli::cli_abort("{.arg centre} has no features")
    }
    if (length(centre) > 1) {
      cli::cli_abort("{.arg centre} has too many features")
    }
    if (!sf::st_is(centre[[1]], "POINT")) {
      cli::cli_abort("{.arg centre} is not a point")
    }
  } else if (inherits(centre, "sfg") && sf::st_is(centre, "POINT")) {
    centre <- sf::st_sfc(centre)
  } else if (is.numeric(centre) && length(centre) == 2) {
    centre <- sf::st_sfc(sf::st_point(centre))
  } else {
    cli::cli_abort(c(
      "{.arg centre} is not in a supported format",
      "i" = "Provide values like an {.fn sf::st_sfc}, {.fn sf::st_point}, or {.code c(0, 0)}."
    ))
  }
  if (is.na(crs_input)) {
    cli::cli_warn(c(
      "{.arg centre} has no coordinate reference system; assuming WGS 84",
      "i" = "Provide {.arg centre} as a {.fn sf::st_sfc} with an explicit {.arg crs} to suppress."
    ))
    centre <- sf::st_set_crs(centre, "EPSG:4326")
  }
  centre
}
