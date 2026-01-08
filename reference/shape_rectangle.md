# Rectangular insets

Rectangular insets

## Usage

``` r
shape_rectangle(centre, hwidth, hheight = NULL)
```

## Arguments

- centre:

  Coordinates of the inset centre. Ideally this should be an `sfc`
  object (see
  [`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html))
  including a coordinate reference system. An
  [`sf::st_point()`](https://r-spatial.github.io/sf/reference/st.html)
  or a vector of longitude and latitude are also accepted. If a CRS
  cannot be determined, WGS 84 is assumed.

- hwidth:

  Half width of the inset in the units of the inset's `crs_working`.

- hheight:

  Half height of the inset in the units of the inset's `crs_working`.
  Defaults to the same value as `hwidth`.

## Value

A shape definition suitable for use with
[`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.md).

## See also

[`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.md)

Other shapes:
[`shape_circle()`](https://cidm-ph.github.io/ggmapinset/reference/shape_circle.md),
[`shape_sf()`](https://cidm-ph.github.io/ggmapinset/reference/shape_sf.md)

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
    geom_sf_inset(map_base = "none") +
    coord_sf_inset(inset = configure_inset(...)) +
    theme_void()
}
rectangle <- shape_rectangle(sf::st_centroid(nc[21,]), hwidth = 50, hheight = 40)
#> Warning: st_centroid assumes attributes are constant over geometries

make_demo(rectangle, scale = 3, translation = c(-300, 0))

make_demo(rectangle, scale = 3, translation = c(-250, -200))

make_demo(rectangle, scale = 3, translation = c(-150, -100))

make_demo(rectangle, scale = 3, translation = c(0, 0))

make_demo(rectangle, scale = 0.5, translation = c(0, 0))
```
