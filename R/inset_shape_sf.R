#' Arbitrary geometry as insets
#'
#' You can use any polygon to define the shape of the inset, including a
#' feature from your base map layer.
#'
#' @param geometry A simple features geometry that is either a polygon or
#'   multipolygon, and is valid and simple.
#' @family shapes
#' @seealso [configure_inset()]
#' @export
#'
#' @examples
#' library(ggplot2)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' make_demo <- function(...) {
#'   ggplot(nc) +
#'     geom_sf(fill = "grey95", colour = "grey85") +
#'     # For a filled frame, we want to interleave it between the base layer
#'     # (above this line) and the target layer (below the following line).
#'     geom_inset_frame(target.aes = list(fill = "white")) +
#'     geom_sf_inset(map_base = "none", colour = NA) +
#'     coord_sf_inset(inset = configure_inset(...)) +
#'     theme_void()
#' }
#' shape <- shape_sf(nc[21,])
#'
#' make_demo(shape, scale = 6, translation = c(-200, -200))
#' make_demo(shape, scale = 6, translation = c(-100, -100))
#' make_demo(shape, scale = 6, translation = c(100, 100))
#' make_demo(shape, scale = 0.5, translation = c(0, 0))
shape_sf <- function(geometry) {
  if (!inherits(geometry, c("sfc", "sf"))) {
    cli::cli_abort("The {.arg geometry} must be an {.cls sfc} or {.cls sf} object")
  }
  geometry <- sf::st_geometry(geometry)
  if (!sf::st_is(geometry, c("POLYGON", "MULTIPOLYGON"))) {
    cli::cli_abort("The {.arg geometry} must be a polygon or multipolygon")
  }
  if (!sf::st_is_valid(geometry)) {
    cli::cli_abort("The {.arg geometry} must be valid")
  }
  if (!sf::st_is_simple(geometry)) {
    cli::cli_abort("The {.arg geometry} must be simple (not self-intersecting)")
  }
  if (is.na(sf::st_crs(geometry))) {
    cli::cli_warn(c(
      "{.arg geometry} has no coordinate reference system; assuming WGS 84",
      "i" = "Specify the CRS using {.fn sf::st_set_crs()} to suppress."
    ))
    geometry <- sf::st_set_crs(geometry, "EPSG:4326")
  }

  structure(
    list(geometry = geometry),
    class = c("shape_sf", "ggmapinset_shape")
  )
}

#' @export
central_point.shape_sf <- function(shape) {
  sf::st_centroid(shape$geometry) |> sf::st_set_crs(sf::st_crs(shape$geometry))
}

#' @importFrom sf st_geometry
#' @export
st_geometry.inset_shape_sf <- function(obj, ...) {
  inset_shape(obj)$geometry
}

#' @export
inset_viewport.inset_shape_sf <- function(inset) {
  sf::st_transform(
    st_geometry(inset),
    inset_crs_working(inset)
  )
}

#' @export
make_frame.inset_shape_sf <- function(inset) {
  crs_working <- inset_crs_working(inset)
  crs_orig <- sf::st_crs(inset_centre(inset))

  centroid <- sf::st_transform(inset_centre(inset), crs_working)
  trans <- inset_translation(inset)
  if (is.null(trans)) trans <- c(0, 0)
  scale <- inset_scale(inset)
  if (is.null(scale)) scale <- 1

  viewport <- inset_viewport(inset)
  result <- viewport

  if (scale != 1) {
    result <- (result - centroid) * scale + centroid
    result <- sf::st_set_crs(result, crs_working)
  }
  if (!is.null(inset_translation(inset))) {
    result <- sf::st_set_crs(result + trans, crs_working)
  }

  lines <- get_quasi_tangents(viewport, result)
  lines <- sf::st_set_crs(lines, crs_working)
  lines <- sf::st_transform(lines, crs_orig)

  viewport <- sf::st_transform(viewport, crs_orig)
  result <- sf::st_transform(result, crs_orig)
  c(viewport, result, lines)
}

get_quasi_tangents <- function(shape1, shape2) {
  # extract key points on the concave hull of the shape
  y1 <- sf::st_intersection(
    sf::st_boundary(shape1),
    sf::st_convex_hull(shape1) |> sf::st_boundary()
  ) |>
    sf::st_cast("POINT")
  y2 <- sf::st_intersection(
    sf::st_boundary(shape2),
    sf::st_convex_hull(shape2) |> sf::st_boundary()
  ) |>
    sf::st_cast("POINT")

  # make every possible line between shapes at those key points
  rays <- apply(
    expand.grid(seq_along(y1), seq_along(y2)), 1,
    simplify = FALSE,
    FUN = function(x) {
      sf::st_linestring(matrix(c(y1[[x[[1]]]], y2[[x[[2]]]]), ncol = 2))
    }
  ) |> sf::st_sfc(crs = sf::st_crs(shape1))

  # exclude rays intersecting the source or target rectangles
  rays <- rays[sf::st_relate(rays, shape1, pattern = "FF1F00***", sparse = FALSE)]
  rays <- rays[sf::st_relate(rays, shape2, pattern = "FF1F00***", sparse = FALSE)]

  if (length(rays) == 0) return(sf::st_sfc(sf::st_multilinestring(), crs = sf::st_crs(shape1)))

  # keep only the shortest ray from each corner
  rays <- rays[order(sf::st_length(rays))]
  rays_keep <- rep(TRUE, length(rays))
  for (i in seq_along(rays)) {
    if (!rays_keep[[i]]) next
    disjoint <- !sf::st_touches(rays, rays[[i]], sparse = FALSE)
    rays_keep[i:length(rays)] <- rays_keep[i:length(rays)] & disjoint[i:length(rays)]
  }
  rays <- rays[rays_keep]

  # keep at most the two rays that are "most separated" by computing the enclosed area
  if (length(rays) > 2) {
    sep <- expand.grid(i = seq_along(rays), j = seq_along(rays))
    sep$area <- apply(sep, 1, simplify = TRUE, FUN = function(x) {
      sf::st_combine(c(rays[[x[[1]]]], rays[[x[[2]]]])) |>
        sf::st_triangulate() |>
        sf::st_area()
    })
    sep_max <- sep[which.max(sep$area), ]
    rays <- rays[c(sep_max$i, sep_max$j)]
  }

  return(sf::st_combine(rays))
}
