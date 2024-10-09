#' @export
inset_viewport.inset_shape_rectangle <- function(inset) {
  centre <- sf::st_transform(inset_centre(inset), inset_crs_working(inset))

  width <- inset_width(inset)
  height <- inset_height(inset)
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
  width <- inset_width(inset)
  height <- inset_width(inset)

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

  # exclude rays intersecting the source rectangle
  rays <- rays[sf::st_relate(rays, r1, pattern = "FF1F00***", sparse = FALSE)]

  if (length(rays) == 0) return(sf::st_sfc(sf::st_multilinestring(), crs = sf::st_crs(r1)))

  # as long as it leaves us with something, also exclude any intersecting the target rectangle
  rays2 <- rays[sf::st_relate(rays, r2, pattern = "FF1F00***", sparse = FALSE)]
  if (length(rays2) > 0) rays <- rays2

  # keep only the shortest ray from each corner
  rays <- rays[order(sf::st_length(rays))]
  rays_keep <- rep(TRUE, length(rays))
  for (i in seq_along(rays)) {
    if (!rays_keep[[i]]) next
    disjoint <- !sf::st_touches(rays, rays[[i]], sparse = FALSE)
    rays_keep[i:length(rays)] <- rays_keep[i:length(rays)] & disjoint[i:length(rays)]
  }
  rays[rays_keep] |> sf::st_combine()
}
