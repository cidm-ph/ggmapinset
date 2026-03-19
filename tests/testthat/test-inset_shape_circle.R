test_that("bitangent has expected multiplicity", {
  centre <- sf::st_centroid(nc[21, ])$geometry[[1]]
  radius <- 50

  bt1 <- get_outer_bitangents(
    centre,
    radius,
    centre + c(-200, -200),
    radius * 3
  )
  expect_length(bt1[[1]], 2)
  bt2 <- get_outer_bitangents(
    centre,
    radius,
    centre + c(-100, -200),
    radius * 3
  )
  expect_length(bt2[[1]], 2)
  bt3 <- get_outer_bitangents(centre, radius, centre, radius * 3)
  expect_length(bt3[[1]], 0)
  bt4 <- get_outer_bitangents(centre, radius, centre, radius * 0.5)
  expect_length(bt4[[1]], 0)
})
