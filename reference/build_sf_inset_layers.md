# Build layers to implement an inset-compatible geometry

For plotting, use
[`geom_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.md)
instead. This helper is intended to be used when implementing custom
geometries based on
[`geom_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.md)
so that they can provide parameters to control the inset.

## Usage

``` r
build_sf_inset_layers(
  data,
  mapping,
  stat,
  position,
  show.legend,
  inherit.aes,
  params,
  inset,
  map_base = "normal",
  map_inset = "auto"
)
```

## Arguments

- data, mapping, stat, position, show.legend, inherit.aes, params:

  See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- inset:

  Inset configuration; see
  [`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.md).
  If `NA` (the default), this is inherited from the coord (see
  [`coord_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/coord_sf_inset.md)).

- map_base:

  Controls the layer with the base map. Possible values are `"normal"`
  to create a layer as though the inset were not specified, `"clip"` to
  create a layer with the inset viewport cut out, and `"none"` to
  prevent the insertion of a layer for the base map.

- map_inset:

  Controls the layer with the inset map. Possible values are `"auto"` to
  choose the behaviour based on whether `inset` is specified, `"normal"`
  to create a layer with the viewport cut out and transformed, and
  `"none"` to prevent the insertion of a layer for the viewport map.

## Value

A `ggplot` layer, or a pair of layers.

## Examples

``` r
my_custom_geom <- function(
  mapping = ggplot2::aes(),
  data = NULL,
  stat = "my_custom_stat",
  position = "identity",
  ...,
  inset = NA,
  map_base = "normal",
  map_inset = "auto",
  na.rm = TRUE,
  inherit.aes = TRUE
) {
  params <- rlang::list2(na.rm = na.rm, ...)
  build_sf_inset_layers(
    data = data, mapping = mapping,
    stat = stat, position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
    inset = inset,
    map_base = map_base,
    map_inset = map_inset
  )
}
```
