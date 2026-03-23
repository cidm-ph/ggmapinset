test_that("inset config does not allow NA", {
  expect_equal(get_inset_config(NULL, list(inset = NULL)), NULL)
  expect_error(get_inset_config(NA, list(inset = NULL)))
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
