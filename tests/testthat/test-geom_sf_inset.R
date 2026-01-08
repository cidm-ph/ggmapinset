test_that("basic inset works", {
  library(ggplot2)
  nc <-
    sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
    # round_sf_geom(100)

  basic_inset <-
    ggplot(nc) +
    geom_sf_inset(aes(fill = AREA), precision = 100) +
    geom_inset_frame(
      source.aes = list(colour = "white"),
      lines.aes = list(colour = "red"),
      target.aes = list(colour = "black", linewidth = 1),
    ) +
    coord_sf_inset(configure_inset(
      shape_rectangle(
        centre = sf::st_sfc(sf::st_point(c(-80, 35.5)), crs = sf::st_crs(nc)),
        hwidth = 50
      ),
      scale = 3, translation = c(-100, -300), units = "km"
    )) +
    theme_void() +
    theme(legend.position = "none")

  expect_doppelganger("basic-inset", basic_inset)
})
