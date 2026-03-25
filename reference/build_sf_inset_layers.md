# Build layers to implement an inset-compatible geometry

For plotting, use
[`geom_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.md)
instead. These helpers are intended to be used when implementing custom
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

get_inset_config(inset, coord)
```

## Arguments

- data, mapping, stat, position, show.legend, inherit.aes, params:

  See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- inset:

  Inset configuration; see
  [`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.md).
  If [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
  the default, this is inherited from the coord (see
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

- coord:

  Coord object for the plot.

## Value

- For `build_sf_inset_layers()`: a `ggplot` layer, or a pair of layers.

- For `get_inset_config()`: a valid inset config object or `NULL`.

## Details

`build_sf_inset_layers()` should be called from a geom constructor
instead of
[`ggplot2::layer_sf()`](https://ggplot2.tidyverse.org/reference/layer_sf.html).
This allows an `inset` parameter to control the creation of two layers
(base + inset) as needed.

`get_inset_config()` should always be called early inside the draw or
compute function, e.g. `Geom$draw_panel()` or `Stat$compute_group()`
whenever that function accepts an `inset` param. The helper validates
the inset configuration after applying fallback to the coord's inset
configuration if needed.

## Examples

``` r
my_custom_geom <- function(
  mapping = ggplot2::aes(),
  data = NULL,
  stat = "my_custom_stat",
  position = "identity",
  ...,
  inset = ggplot2::waiver(),
  map_base = "normal",
  map_inset = "auto",
  na.rm = TRUE,
  inherit.aes = TRUE
) {
  params <- rlang::list2(na.rm = na.rm, ...)
  build_sf_inset_layers(
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
    inset = inset,
    map_base = map_base,
    map_inset = map_inset
  )
}

# defining a new geom deriving from geom_sf()
GeomCustom <- ggplot2::ggproto("GeomCustom", ggplot2::GeomSf,
  draw_panel = function(self, data, panel_params, coord, inset = ggplot2::waiver()) {
    inset <- get_inset_config(inset, coord)

    # do something with the inset ...

    # note that this example doesn't pass on the remaining geom_sf params but
    # in real usage you would probably want to do that
    ggplot2::ggproto_parent(ggplot2::GeomSf, self)$draw_panel(data, panel_params, coord)
  },
)
```
