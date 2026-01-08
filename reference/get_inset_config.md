# Get the inset configuration from the params or coord

This is a helper for implementing inset-aware ggplot layers. If the
`inset` is missing (`NA`) then the default inset configuration is
retrieved from the coord.

## Usage

``` r
get_inset_config(inset, coord)
```

## Arguments

- inset:

  Inset passed in as a param to the layer

- coord:

  Coord object for the plot

## Value

Inset configuration or `NULL`

## Examples

``` r
# defining a new geom deriving from geom_sf()
GeomCustom <- ggplot2::ggproto("GeomCustom", ggplot2::GeomSf,
  draw_panel = function(self, data, panel_params, coord, inset = NA) {
    inset <- get_inset_config(inset, coord)

    # do something with the inset ...

    # note that this example doesn't pass on the remaining geom_sf params but
    # in real usage you would probably want to do that
    ggplot2::ggproto_parent(ggplot2::GeomSf, self)$draw_panel(data, panel_params, coord)
  },
)
```
