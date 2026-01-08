# Arbitrary geometry as insets

You can use any polygon to define the shape of the inset, including a
feature from your base map layer.

## Usage

``` r
shape_sf(geometry)
```

## Arguments

- geometry:

  A simple features geometry that is either a polygon or multipolygon,
  and is valid and simple.

## Value

A shape definition suitable for use with
[`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.md).

## See also

[`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.md)

Other shapes:
[`shape_circle()`](https://cidm-ph.github.io/ggmapinset/reference/shape_circle.md),
[`shape_rectangle()`](https://cidm-ph.github.io/ggmapinset/reference/shape_rectangle.md)

## Examples

``` r
library(ggplot2)
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
make_demo <- function(...) {
  ggplot(nc) +
    geom_sf(fill = "grey95", colour = "grey85") +
    # For a filled frame, we want to interleave it between the base layer
    # (above this line) and the target layer (below the following line).
    geom_inset_frame(target.aes = list(fill = "white")) +
    geom_sf_inset(map_base = "none", colour = NA) +
    coord_sf_inset(inset = configure_inset(...)) +
    theme_void()
}
shape <- shape_sf(nc[21,])

make_demo(shape, scale = 6, translation = c(-200, -200))
#> Warning: 'x' is NULL so the result will be NULL

make_demo(shape, scale = 6, translation = c(-100, -100))
#> Warning: 'x' is NULL so the result will be NULL

make_demo(shape, scale = 6, translation = c(100, 100))
#> Warning: 'x' is NULL so the result will be NULL

make_demo(shape, scale = 0.5, translation = c(0, 0))
#> Warning: 'x' is NULL so the result will be NULL
```
