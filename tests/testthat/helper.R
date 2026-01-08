library(vdiffr)

round_sf_geom <- function(x, precision) {
  new_geom <-
    sf::st_geometry(x) |>
    sf::st_set_precision(precision) |>
    sf::st_as_binary() |>
    sf::st_as_sfc(crs = sf::st_crs(x))
  sf::st_set_geometry(x, new_geom)
}
