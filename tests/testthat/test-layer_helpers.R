test_that("inset config inherits correctly from coord", {
  point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = "EPSG:3857")
  config_a <- configure_inset(shape_circle(point, 1))
  config_b <- configure_inset(shape_circle(point, 2))
  coord <- structure(list(inset = config_b), class = "CoordSfInset")
  coord_empty <- structure(list(), class = "CoordSf")

  expect_equal(get_inset_config(config_a, coord_empty), config_a)
  expect_equal(get_inset_config(config_a, coord), config_a)
  expect_equal(get_inset_config(NULL, coord_empty), NULL)
  expect_equal(get_inset_config(NULL, coord), NULL)
  expect_equal(get_inset_config(waiver(), coord_empty), NULL)
  expect_equal(get_inset_config(waiver(), coord), config_b)

  # NA deprecated but works like waiver()
  expect_error(
    rlang::with_options(
      lifecycle_verbosity = "error",
      get_inset_config(NA, coord_empty)
    ),
    ".*inset.* must not be NA*"
  )
  expect_equal(get_inset_config(NA, coord_empty), NULL)
  expect_equal(get_inset_config(NA, coord), config_b)
})

test_that("inset is disabled when inset=NULL", {
  # Case 1. layer: config, coord: N/A
  #    -> should generate 2 real layers
  p1 <- ggplot(nc) +
    geom_sf_inset(inset = configure_inset(shape_circle(c(-80, 35.5), 1)))
  expect_length(p1@layers, 2)
  expect_s3_class(ggplot2::get_layer_grob(p1, 2)[[1]], "pathgrob")

  # Case 2. layer: NULL, coord: N/A
  #    -> should generate 1 real layer only
  p2 <- ggplot(nc) + geom_sf_inset(inset = NULL)
  expect_length(p2@layers, 1)

  # Case 3. layer: waiver, coord: not set
  #    -> should generate 1 real layer and one no-op layer
  p3 <- ggplot(nc) + geom_sf_inset(inset = waiver())
  expect_length(p3@layers, 2)
  expect_s3_class(ggplot2::get_layer_grob(p3, 2)[[1]], "null")

  # Case 4. layer: waiver, coord: config
  #    -> should generate 2 real layers
  p4 <- ggplot(nc) +
    geom_sf_inset(inset = waiver()) +
    coord_sf_inset(inset = configure_inset(shape_circle(c(-80, 35.5), 1)))
  expect_length(p4@layers, 2)
  expect_s3_class(ggplot2::get_layer_grob(p4, 2)[[1]], "pathgrob")

  # Case 5. layer: waiver, coord: NULL
  #    -> should generate 1 real layer and one no-op layer
  p5 <- ggplot(nc) +
    geom_sf_inset(inset = waiver()) +
    coord_sf_inset(inset = NULL)
  expect_length(p5@layers, 2)
  expect_s3_class(ggplot2::get_layer_grob(p5, 2)[[1]], "null")
})
