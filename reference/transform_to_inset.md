# Transform coordinates according to inset configuration

This helper operates on an sf object to scale and translate its geometry
according to the inset specification.

## Usage

``` r
transform_to_inset(x, inset)
```

## Arguments

- x:

  Spatial data frame or other sf object; see
  [`sf::st_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.html).

- inset:

  Inset configuration; see
  [`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.md).

## Value

A copy of `x` with the geometry replaced by the transformed version.

## Examples

``` r
library(sf)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
cfg <- configure_inset(
  centre = st_sfc(st_point(c(-82, 35)), crs = 4326),
  scale = 2,
  translation = c(10, -60),
  radius = 50,
  units = "mi")

transform_to_inset(nc, cfg)
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -86.47107 ymin: 31.89666 xmax: -68.73731 ymax: 37.31197
#> Geodetic CRS:  NAD27
#> First 10 features:
#>     AREA PERIMETER CNTY_ CNTY_ID        NAME  FIPS FIPSNO CRESS_ID BIR74 SID74
#> 1  0.114     1.442  1825    1825        Ashe 37009  37009        5  1091     1
#> 2  0.061     1.231  1827    1827   Alleghany 37005  37005        3   487     0
#> 3  0.143     1.630  1828    1828       Surry 37171  37171       86  3188     5
#> 4  0.070     2.968  1831    1831   Currituck 37053  37053       27   508     1
#> 5  0.153     2.206  1832    1832 Northampton 37131  37131       66  1421     9
#> 6  0.097     1.670  1833    1833    Hertford 37091  37091       46  1452     7
#> 7  0.062     1.547  1834    1834      Camden 37029  37029       15   286     0
#> 8  0.091     1.284  1835    1835       Gates 37073  37073       37   420     0
#> 9  0.118     1.421  1836    1836      Warren 37185  37185       93   968     4
#> 10 0.124     1.428  1837    1837      Stokes 37169  37169       85  1612     1
#>    NWBIR74 BIR79 SID79 NWBIR79                       geometry
#> 1       10  1364     0      19 MULTIPOLYGON (((-80.76887 3...
#> 2       10   542     3      12 MULTIPOLYGON (((-80.30314 3...
#> 3      208  3616     6     260 MULTIPOLYGON (((-78.73605 3...
#> 4      123   830     2     145 MULTIPOLYGON (((-69.8413 36...
#> 5     1066  1606     3    1197 MULTIPOLYGON (((-72.25869 3...
#> 6      954  1838     5    1237 MULTIPOLYGON (((-71.31349 3...
#> 7      115   350     2     139 MULTIPOLYGON (((-69.8413 36...
#> 8      254   594     2     371 MULTIPOLYGON (((-70.94837 3...
#> 9      748  1190     2     844 MULTIPOLYGON (((-74.44088 3...
#> 10     160  2038     5     176 MULTIPOLYGON (((-77.87471 3...
```
