#' Configure transformations underpinning a map inset
#'
#' The configuration returned by this function will normally be passed to the
#' coordinate system via [coord_sf_inset()]. Insets can either be circular
#' (if \code{radius} is specified) or rectangular (if \code{hwidth} and
#' optionally \code{hheight} are specified).
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
#' @param centre Coordinates of the inset centre. Ideally this should be an
#'   \code{sfc} object (see [sf::st_sfc()]) including a coordinate reference system.
#'   An [sf::st_point()] or a vector of longitude and latitude are also accepted.
#'   If a CRS cannot be determined, \code{crs_working} is assumed.
#' @param scale Zoom scale: values larger than one will make the inset bigger.
#' @param translation Translation (shift) of the inset relative to the centre.
#'   This can be an \code{st_point} or simply a vector of length 2 containing
#'   the x and y offsets respectively. Units are specified by \code{crs_working}.
#' @param radius Radius of the inset circle in the units of \code{crs_working}.
#'   Cannot be used with \code{hwidth}.
#' @param hwidth Half width of the inset in the units of \code{crs_working}.
#'   Cannot be used with \code{radius}.
#' @param hheight Half height of the inset in the units of \code{crs_working}.
#'   Defaults to the same value as `hwidth`.
#' @param units Base length unit (e.g. \code{"km"} or \code{"mi"}). Ignored if
#'   \code{crs_working} is provided. See Details for supported values.
#' @param crs_working The coordinate reference system to use internally when
#'   applying the transformations. See Details.
#'
#' @returns An inset configuration object of class \code{inset_config}.
#' @export
#'
#' @examples
#' library(sf)
#'
#' # circular inset with a 2x enlargement
#' cfg <- configure_inset(
#'   centre = st_sfc(st_point(c(-82, 35)), crs = 4326),
#'   scale = 2,
#'   translation = c(70, -180),
#'   radius = 50,
#'   units = "mi")
configure_inset <- function(
  centre,
  scale = NULL,
  translation = NULL,
  radius = NULL,
  hwidth = NULL,
  hheight = NULL,
  units = "km",
  crs_working = NULL
) {
  crs_input <- sf::NA_crs_
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
    cli::cli_abort(c("{.arg centre} is not in a supported format",
                     "i" = "Provide values like an {.fn sf::st_sfc}, {.fn sf::st_point}, or {c(0, 0)}."))
  }

  if (is.na(crs_input)) {
    cli::cli_warn(c("{.arg centre} has no coordinate reference system; assuming WGS 84"),
                  "i" = "Provide {.arg centre} as a {.fn sf::st_sfc} with an explicit {.arg crs} to suppress.")
    centre <- sf::st_set_crs(centre, "EPSG:4326")
  }

  if (is.null(crs_working)) {
    lat <- if (sf::st_is_longlat(centre)) {
      centre[[1]][[2]]
    } else {
      sf::st_transform(centre, "EPSG:4326")[[1]][[2]]
    }
    crs_working <- paste0("+proj=eqc", " +units=", units, " +lat_ts=", lat)
  }

  make_inset_config(list(
    centre = centre,
    scale = scale,
    translation = translation,
    radius = radius,
    hwidth = hwidth,
    hheight = hheight,
    crs_working = crs_working
  ))
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
  check_inset_config(inset)
  structure(list(inset), class = c(paste0("inset_shape_", guess_inset_shape(inset)), "inset_config"))
}

#' @export
make_inset_config.inset_config <- function(inset) {
  inset
}

inset_crs_working <- function(inset) {
  sf::st_crs(inset[[1]]$crs_working)
}

inset_radius <- function(inset) {
  inset[[1]]$radius
}

inset_centre <- function(inset) {
  inset[[1]]$centre
}

inset_width <- function(inset) {
  inset[[1]]$hwidth
}

inset_height <- function(inset) {
  h <- inset[[1]]$hheight
  if (!is.null(h)) return(h)
  inset[[1]]$hwidth
}

inset_scale <- function(inset) {
  inset[[1]]$scale
}

inset_translation <- function(inset) {
  inset[[1]]$translation
}

guess_inset_shape <- function(inset) {
  if (!is.null(inset[["radius"]])) return("circle")
  if (!is.null(inset[["hwidth"]])) return("rectangle")
  stop("Invalid shape")
}

check_inset_config <- function(inset) {
  if (is.null(inset)) cli::cli_abort("Inset configuration must be provided")

  if (!is.null(inset[["radius"]]) && !is.null(inset[["hwidth"]])) {
    cli::cli_abort("Only one of inset {.arg radius} or {.arg hwidth} can be specified")
  }
  if (!is.null(inset[["radius"]])) {
    if (inset$radius <= 0) {
      cli::cli_abort("Inset {.arg radius} must be a positive number, not {inset$radius}")
    }
  } else if (!is.null(inset[["hwidth"]])) {
    if (inset$hwidth <= 0) {
      cli::cli_abort("Inset {.arg hwidth} must be a positive number, not {inset$hwidth}")
    }
  } else {
    cli::cli_abort(c("Unable to determine inset shape", "i" = "Specify inset {.arg radius} or {.arg hwidth}"))
  }

  if (is.null(inset[["crs_working"]])) {
    cli::cli_abort("Inset {.arg crs_working} must be provided")
  }

  if (is.null(inset[["centre"]])) {
    cli::cli_abort("Inset {.arg centre} must be provided")
  }
  if (!inherits(inset$centre, "sfc") || is.na(sf::st_crs(inset$centre))) {
    cli::cli_abort("Inset {.arg centre} must be an sfc object with a CRS")
  }
}
