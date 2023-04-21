
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggmapinset

<!-- badges: start -->

[![r-universe
status](https://cidm-ph.r-universe.dev/badges/ggmapinset)](https://cidm-ph.r-universe.dev)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggmapinset)](https://CRAN.R-project.org/package=ggmapinset)
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

`{ggmapinset}` provides replacements for each of the sf-related layers
from `{ggplot2}` with the suffix `_inset` added to the names. For the
most part they work the same way as the standard layers, but they can
understand the inset configuration and make small adjustments
accordingly such as expanding axes to fit the inset frame or duplicating
the layer and transforming it to fit into the inset viewport.

## Example

This example adds an inset to the first example from `ggplot2::geom_sf`.
The inset area is defined as a circle centred on the named county, with
radius 50 miles. The inset is enlarged by a factor of 2 and shifted to
an empty part of the map.

``` r
library(ggmapinset)
library(ggplot2)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# The basic ggplot example:
# ggplot(nc) +
#   geom_sf(aes(fill = AREA)) +
#   coord_sf()

# Adding an inset means replacing geom_sf(...) -> geom_sf_inset(...) and
# coord_sf(...) -> coord_sf_inset(..., inset = configure_inset(...))
ggplot(nc) +
  geom_sf_inset(aes(fill = AREA)) +
  geom_inset_frame() +
  geom_sf_label_inset(aes(label = NAME), data = ~dplyr::slice_sample(.x, n = 10)) +
  coord_sf_inset(inset = configure_inset(
    centre = sf::st_centroid(sf::st_geometry(nc)[nc$NAME == "Yancey"]),
    scale = 2, translation = c(70, -180), radius = 50, units = "mi"))
```

<img src="man/figures/README-example-1.png" width="100%" />
