#' Configure transformations underpinning a map inset
#'
#' The configuration returned by this function will often be passed to the
#' coordinate system via [coord_sf_inset()] so that it is propagated to all
#' relevant layers.
#'
#' The default \code{crs_working} uses the equidistant cylindrical coordinate
#' reference system with the latitude of true scale set to match the latitude of
#' \code{centre}. This ensures that circular insets will appear circular in most
#' cases since the projection is not distorted near the centre. The geometries
#' are converted to this CRS for the inset transformation and constructing the
#' inset frame, and are converted back to the CRS of \code{centre} at the end.
#'
#' The default units are kilometres but can be changed with \code{units}
#' instead of specifying the whole projection. The possible values for
#' \code{units} are
#' \href{https://proj.org/operations/conversions/unitconvert.html#distance-units}{those understood by \code{proj}}:
#' ```{r, echo=FALSE, results="asis"}
#' cat("\n\\itemize{\n")
#' units <- read.table(text = system2("proj", "-lu", stdout = TRUE), fill = TRUE)
#' desc <- apply(units[,3:ncol(units)], MARGIN = 1,
#'               FUN = function (x) trimws(paste0(x, collapse = " ")))
#' units <- units[,1]
#' cat(paste0("\\item \\code{", '"', units, '"', "}: ", desc, collapse = "\n"))
#' cat("}\n")
#' ```
#'
#' @param shape Inset shape: see [shape_circle()], [shape_rectangle()],
#'   or [shape_sf()].
#' @param scale Zoom scale: values larger than one will make the inset bigger.
#' @param translation Translation (shift) of the inset relative to the centre.
#'   This can be an `sf::st_point()` or simply a vector of length 2 containing
#'   the x and y offsets respectively. Units are specified by `crs_working`.
#' @param units Base length unit (e.g. `"km"` or `"mi"`). Ignored if
#'   `crs_working` is provided. See Details for supported values.
#' @param crs_working The coordinate reference system to use internally when
#'   applying the transformations. See Details.
#' @param centre,radius `r lifecycle::badge("deprecated")`
#'   Use `shape = shape_circle(centre, radius)` instead.
#'
#' @returns An inset configuration object of class \code{inset_config}.
#' @export
#'
#' @examples
#' library(sf)
#'
#' # circular inset with a 2x enlargement
#' cfg <- configure_inset(
#'   shape_circle(centre = c(-82, 35), radius = 50),
#'   scale = 2,
#'   translation = c(70, -180),
#'   units = "mi"
#' )
configure_inset <- function(
  shape,
  scale = NULL,
  translation = NULL,
  units = "km",
  crs_working = NULL,
  centre = deprecated(),
  radius = deprecated()
) {
  if (lifecycle::is_present(radius)) {
    lifecycle::deprecate_warn(
      when = "0.4.0",
      what = "configure_inset(radius)",
      details = "Use `shape = shape_circle(centre, radius)` instead."
    )
    shape <- shape_circle(centre = centre, radius = radius)
  }

  if (is.null(crs_working)) {
    point <- central_point(shape)
    lat <- if (sf::st_is_longlat(point)) {
      point[[1]][[2]]
    } else {
      sf::st_transform(point, "EPSG:4326")[[1]][[2]]
    }
    crs_working <- paste0("+proj=eqc", " +units=", units, " +lat_ts=", lat)
  }

  make_inset_config(list(
    shape = shape,
    scale = scale,
    translation = translation,
    crs_working = crs_working
  ))
}

is_shape <- function(x) {
  inherits(x, "ggmapinset_shape")
}

is_inset_config <- function(x) {
  inherits(x, "inset_config")
}

make_inset_config <- function(inset) {
  UseMethod("make_inset_config")
}

#' @export
make_inset_config.NULL <- function(inset) {
  NULL
}

#' @export
make_inset_config.list <- function(inset) {
  if (is.null(inset)) cli::cli_abort("Inset configuration must be provided")
  if (!is_shape(inset[["shape"]])) {
    cli::cli_abort("Inset {.arg shape} must be a known shape")
  }
  if (is.null(inset[["crs_working"]])) {
    cli::cli_abort("Inset {.arg crs_working} must be provided")
  }

  structure(
    list(inset),
    class = c(paste0("inset_", class(inset[["shape"]])[[1]]), "inset_config")
  )
}

#' @export
make_inset_config.inset_config <- function(inset) {
  inset
}

inset_crs_working <- function(inset) {
  sf::st_crs(inset[[1]]$crs_working)
}

inset_shape <- function(inset) {
  inset[[1]]$shape
}

inset_centre <- function(inset) {
  central_point(inset[[1]]$shape)
}

inset_scale <- function(inset) {
  inset[[1]]$scale
}

inset_translation <- function(inset) {
  inset[[1]]$translation
}

coerce_centre <- function(centre) {
  if (is.null(centre)) {
    cli::cli_abort("Shape {.arg centre} must be provided")
  }
  crs_input <- sf::NA_crs_
  if (inherits(centre, "sf")) {
    centre <- sf::st_geometry(centre)
  }
  if (inherits(centre, "sfc")) {
    crs_input <- sf::st_crs(centre)
    if (length(centre) == 0) cli::cli_abort("{.arg centre} has no features")
    if (length(centre) > 1) cli::cli_abort("{.arg centre} has too many features")
    if (!sf::st_is(centre[[1]], "POINT")) cli::cli_abort("{.arg centre} is not a point")
  } else if (inherits(centre, "sfg") && sf::st_is(centre, "POINT")) {
    centre <- sf::st_sfc(centre)
  } else if (is.numeric(centre) && length(centre) == 2) {
    centre <- sf::st_sfc(sf::st_point(centre))
  } else {
    cli::cli_abort(c(
      "{.arg centre} is not in a supported format",
      "i" = "Provide values like an {.fn sf::st_sfc}, {.fn sf::st_point}, or {c(0, 0)}."
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

# Return one point that can be used to choose a sensible working CRS
central_point <- function(shape) {
  UseMethod("central_point")
}
