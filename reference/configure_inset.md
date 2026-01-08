# Configure transformations underpinning a map inset

The configuration returned by this function will often be passed to the
coordinate system via
[`coord_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/coord_sf_inset.md)
so that it is propagated to all relevant layers.

## Usage

``` r
configure_inset(
  shape,
  scale = NULL,
  translation = NULL,
  units = "km",
  crs_working = NULL,
  centre = deprecated(),
  radius = deprecated()
)
```

## Arguments

- shape:

  Inset shape: see
  [`shape_circle()`](https://cidm-ph.github.io/ggmapinset/reference/shape_circle.md),
  [`shape_rectangle()`](https://cidm-ph.github.io/ggmapinset/reference/shape_rectangle.md),
  or
  [`shape_sf()`](https://cidm-ph.github.io/ggmapinset/reference/shape_sf.md).

- scale:

  Zoom scale: values larger than one will make the inset bigger.

- translation:

  Translation (shift) of the inset relative to the centre. This can be
  an
  [`sf::st_point()`](https://r-spatial.github.io/sf/reference/st.html)
  or simply a vector of length 2 containing the x and y offsets
  respectively. Units are specified by `crs_working`.

- units:

  Base length unit (e.g. `"km"` or `"mi"`). Ignored if `crs_working` is
  provided. See Details for supported values.

- crs_working:

  The coordinate reference system to use internally when applying the
  transformations. See Details.

- centre, radius:

  **\[deprecated\]** Use `shape = shape_circle(centre, radius)` instead.

## Value

An inset configuration object of class `inset_config`.

## Details

The default `crs_working` uses the equidistant cylindrical coordinate
reference system with the latitude of true scale set to match the
latitude of `centre`. This ensures that circular insets will appear
circular in most cases since the projection is not distorted near the
centre. The geometries are converted to this CRS for the inset
transformation and constructing the inset frame, and are converted back
to the CRS of `centre` at the end.

The default units are kilometres but can be changed with `units` instead
of specifying the whole projection. The possible values for `units` are
[those understood by
`proj`](https://proj.org/operations/conversions/unitconvert.html#distance-units):

- `"mm"`: millimetre

- `"cm"`: centimetre

- `"m"`: metre

- `"ft"`: foot

- `"us-ft"`: US survey foot

- `"fath"`: fathom

- `"kmi"`: nautical mile

- `"us-ch"`: US survey chain

- `"us-mi"`: US survey mile

- `"km"`: kilometre

- `"ind-ft"`: Indian foot (1937)

- `"ind-yd"`: Indian yard (1937)

- `"mi"`: Statute mile

- `"yd"`: yard

- `"ch"`: chain

- `"link"`: link

- `"dm"`: decimeter

- `"in"`: inch

- `"ind-ch"`: Indian chain

- `"us-in"`: US survey inch

- `"us-yd"`: US survey yard

## Examples

``` r
library(sf)

# circular inset with a 2x enlargement
cfg <- configure_inset(
  shape_circle(centre = c(-82, 35), radius = 50),
  scale = 2,
  translation = c(70, -180),
  units = "mi"
)
#> Warning: `centre` has no coordinate reference system; assuming WGS 84
#> ℹ Provide `centre` as a `sf::st_sfc()` with an explicit `crs` to suppress.
```
