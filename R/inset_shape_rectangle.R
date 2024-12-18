#' Rectangular insets
#'
#' @param centre Coordinates of the inset centre. Ideally this should be an
#'   \code{sfc} object (see [sf::st_sfc()]) including a coordinate reference system.
#'   An [sf::st_point()] or a vector of longitude and latitude are also accepted.
#'   If a CRS cannot be determined, WGS 84 is assumed.
#' @param hwidth Half width of the inset in the units of the inset's `crs_working`.
#' @param hheight Half height of the inset in the units of the inset's `crs_working`.
#'   Defaults to the same value as `hwidth`.
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
#'     geom_sf_inset(map_base = "none") +
#'     coord_sf_inset(inset = configure_inset(...)) +
#'     theme_void()
#' }
#' rectangle <- shape_rectangle(sf::st_centroid(nc[21,]), hwidth = 50, hheight = 40)
#'
#' make_demo(rectangle, scale = 3, translation = c(-300, 0))
#' make_demo(rectangle, scale = 3, translation = c(-250, -200))
#' make_demo(rectangle, scale = 3, translation = c(-150, -100))
#' make_demo(rectangle, scale = 3, translation = c(0, 0))
#' make_demo(rectangle, scale = 0.5, translation = c(0, 0))
shape_rectangle <- function(centre, hwidth, hheight = NULL) {
  centre <- coerce_centre(centre)
  if (hwidth <= 0) {
    cli::cli_abort("Rectangle {.arg hwidth} must be a positive number, not {hwidth}")
  }
  if (is.null(hheight)) {
    hheight <- hwidth
  }

  structure(
    list(centre = centre, hwidth = hwidth, hheight = hheight),
    class = c("shape_rectangle", "ggmapinset_shape")
  )
}

#' @export
central_point.shape_rectangle <- function(shape) {
  shape$centre
}

#' @export
inset_viewport.inset_shape_rectangle <- function(inset) {
  centre <- sf::st_transform(inset_centre(inset), inset_crs_working(inset))

  width <- inset_shape(inset)$hwidth
  height <- inset_shape(inset)$hheight
  c(
    centre + c(-width, +height), # NW
    centre + c(+width, +height), # NE
    centre + c(+width, -height), # SE
    centre + c(-width, -height), # SW
    centre + c(-width, +height)  # NW
  ) |>
    sf::st_combine() |>
    sf::st_cast("POLYGON") |>
    sf::st_set_crs(sf::st_crs(centre))
}

#' @export
make_frame.inset_shape_rectangle <- function(inset) {
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

  lines <- get_burst_lines(viewport, result)
  lines <- sf::st_set_crs(lines, crs_working)
  lines <- sf::st_transform(lines, crs_orig)

  viewport <- sf::st_transform(viewport, crs_orig)
  result <- sf::st_transform(result, crs_orig)
  c(viewport, result, lines)
}

get_burst_lines <- function(r1, r2) {
  y1 <- sf::st_cast(r1, "POINT")
  y2 <- sf::st_cast(r2, "POINT")
  rays <- apply(expand.grid(1:4, 1:4), 1, simplify = FALSE, FUN = function(x) {
    sf::st_linestring(matrix(c(y1[[x[[1]]]], y2[[x[[2]]]]), ncol = 2))
  }) |> sf::st_sfc(crs = sf::st_crs(r1))

  # exclude rays intersecting the source or target rectangles
  rays <- rays[sf::st_relate(rays, r1, pattern = "FF1F00***", sparse = FALSE)]
  rays <- rays[sf::st_relate(rays, r2, pattern = "FF1F00***", sparse = FALSE)]

  if (length(rays) == 0) return(sf::st_sfc(sf::st_multilinestring(), crs = sf::st_crs(r1)))

  # keep only the shortest ray from each corner
  rays <- rays[order(sf::st_length(rays))]
  rays_keep <- rep(TRUE, length(rays))
  for (i in seq_along(rays)) {
    if (!rays_keep[[i]]) next
    disjoint <- !sf::st_touches(rays, rays[[i]], sparse = FALSE)
    rays_keep[i:length(rays)] <- rays_keep[i:length(rays)] & disjoint[i:length(rays)]
  }
  rays <- rays[rays_keep]

  # keep at most the two longest remaining rays
  if (length(rays) > 2) {
    rays <- rays[order(sf::st_length(rays), decreasing = TRUE)[1:2]]
  }

  sf::st_combine(rays)
}
