# ggmapinset

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

[ggmapinset](https://github.com/cidm-ph/ggmapinset) provides drop-in
replacements for each of the
[sf](https://r-spatial.github.io/sf/)-related layers from
[ggplot2](https://ggplot2.tidyverse.org):

| `ggplot2` function                                                                          | `ggmapinset` replacement                                                                                     |
|:--------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------|
| [`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)                            | [`geom_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.md)                         |
| [`geom_sf_text()`](https://ggplot2.tidyverse.org/reference/ggsf.html)                       | [`geom_sf_text_inset()`](https://cidm-ph.github.io/ggmapinset/reference/stat_sf_coordinates_inset.md)        |
| [`geom_sf_label()`](https://ggplot2.tidyverse.org/reference/ggsf.html)                      | [`geom_sf_label_inset()`](https://cidm-ph.github.io/ggmapinset/reference/stat_sf_coordinates_inset.md)       |
| [`stat_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)                            | [`stat_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.md)                         |
| [`stat_sf_coordinates()`](https://ggplot2.tidyverse.org/reference/stat_sf_coordinates.html) | [`stat_sf_coordinates_inset()`](https://cidm-ph.github.io/ggmapinset/reference/stat_sf_coordinates_inset.md) |
| [`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)                           | [`coord_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/coord_sf_inset.md)                       |

The replacements work the same as their normal versions but copy, zoom,
and clip the layers to make the inset work. The stats can be used to add
inset support to geoms from third-party packages. For extension
developers, tools are provided to make
[sf](https://r-spatial.github.io/sf/)-based layers inset-aware (see
[ggautomap](https://github.com/cidm-ph/ggautomap) for examples).

## Example

This example adds an inset to the first example from
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).
The inset area is defined as a circle centred on the named county, with
radius 50 miles. The inset is enlarged by a factor of 2 and shifted to
an empty part of the map.

``` r
library(ggmapinset)
library(ggplot2)

# load the North Carolina map example shipped with sf
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

inset_cfg <- configure_inset(
  shape_circle(
    centre =  sf::st_centroid(nc[nc$NAME == "Yancey",]),
    radius = 50
  ),
  scale = 2,
  units = "mi",
  translation = c(70, -180)
)
#> Warning: st_centroid assumes attributes are constant over geometries

# pick some counties to label
labelled_counties <- sample(nc$NAME, 10)
data_subset <- function(df) df[df$NAME %in% labelled_counties,]
```

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

![](reference/figures/README-example-1.png)

For more information, see the [online
documentation](https://cidm-ph.github.io/ggmapinset/) and
[`vignette("ggmapinset")`](https://cidm-ph.github.io/ggmapinset/articles/ggmapinset.md).

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
