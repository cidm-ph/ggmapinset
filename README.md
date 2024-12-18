
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggmapinset

<!-- badges: start -->

[![r-universe
status](https://cidm-ph.r-universe.dev/badges/ggmapinset)](https://cidm-ph.r-universe.dev)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggmapinset)](https://CRAN.R-project.org/package=ggmapinset)
[![R-CMD-check](https://github.com/cidm-ph/ggmapinset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cidm-ph/ggmapinset/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Add zoomed inset panels to your ggplot maps.

## Installation

You can install ggmapinset like so:

``` r
# CRAN release
install.packages('ggmapinset')

# development version
install.packages('ggmapinset', repos = c('https://cidm-ph.r-universe.dev', 'https://cloud.r-project.org'))
```

## Replacing ‘ggplot2’ sf layers

`{ggmapinset}` provides drop-in replacements for each of the
`{sf}`-related layers from `{ggplot2}`:

| `ggplot2` function      | `ggmapinset` replacement      |
|:------------------------|:------------------------------|
| `geom_sf()`             | `geom_sf_inset()`             |
| `geom_sf_text()`        | `geom_sf_text_inset()`        |
| `geom_sf_label()`       | `geom_sf_label_inset()`       |
| `stat_sf()`             | `stat_sf_inset()`             |
| `stat_sf_coordinates()` | `stat_sf_coordinates_inset()` |
| `coord_sf()`            | `coord_sf_inset()`            |

The replacements work the same as their normal versions but copy, zoom,
and clip the layers to make the inset work. The stats can be used to add
inset support to geoms from third-party packages. For extension
developers, tools are provided to make `{sf}`-based layers inset-aware
(see `{ggautomap}` for examples).

## Example

This example adds an inset to the first example from
`ggplot2::geom_sf()`. The inset area is defined as a circle centred on
the named county, with radius 50 miles. The inset is enlarged by a
factor of 2 and shifted to an empty part of the map.

``` r
library(ggmapinset)
library(ggplot2)

# load the North Carolina map example shipped with sf
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# find the centroid of the specified county
inset_centre <- sf::st_centroid(sf::st_geometry(nc)[nc$NAME == "Yancey"])

inset_cfg <- configure_inset(
  shape_circle(
    centre = inset_centre,
    radius = 50
  ),
  scale = 2,
  units = "mi",
  translation = c(70, -180)
)

# pick some counties to label
labelled_counties <- sample(nc$NAME, 10)
data_subset <- function(df) df[df$NAME %in% labelled_counties,]
```

<div style="display: flex; gap: 1em;">

<div style="flex-grow: 1">

``` r
# base ggplot
ggplot(nc) +
  geom_sf(aes(fill = AREA)) +
  
  geom_sf_label(
    aes(label = NAME),
    data = data_subset
  ) +
  coord_sf()
```

</div>

<div style="flex-grow: 1">

``` r
# with inset added
ggplot(nc) +
  geom_sf_inset(aes(fill = AREA)) +
  geom_inset_frame() +
  geom_sf_label_inset(
    aes(label = NAME),
    data = data_subset
  ) +
  coord_sf_inset(inset_cfg)
```

</div>

</div>

<img src="man/figures/README-example-1.png" width="100%" />

For more information, see the [online
documentation](https://cidm-ph.github.io/ggmapinset/) and
`vignette("ggmapinset")`.

## Limitations

The package implements insets by duplicating and transforming spatial
data within a single coordinate system. That means that you don’t get
separate grid lines for the inset panel and, more significantly, the
inset can be distorted by the projection of the base map if you move it
too far. This tends not to be a problem in practice if you choose a
coordinate system that isn’t too distorted over the area of the base
map.

## Alternatives

Other packages implement different approaches:

- [`facet_zoom()`](https://ggforce.data-imaginist.com/reference/facet_zoom.html)
  from [ggforce](https://cran.r-project.org/package=ggforce)
- [`geom_magnify()`](https://hughjonesd.github.io/ggmagnify/reference/geom_magnify.html)
  from [ggmagnify](https://github.com/hughjonesd/ggmagnify/)
- [`mf_inset_on()`](https://riatelab.github.io/mapsf/reference/mf_inset_on.html)
  from [mapsf](https://cran.r-project.org/package=mapsf)

There are also several articles describing more manual ways to achieve
different insets:

- <https://www.datawim.com/post/inset-map-in-r/>
- <https://dieghernan.github.io/202203_insetmaps/>
- <https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/>
