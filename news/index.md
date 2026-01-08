# Changelog

## ggmapinset (development version)

- Housekeeping: correct minimum R version, suppress a spurious warning.

## ggmapinset 0.4.0

CRAN release: 2024-12-18

- Added support for rectangular insets via
  [`shape_rectangle()`](https://cidm-ph.github.io/ggmapinset/reference/shape_rectangle.md).
- Added support for arbitrary insets via
  [`shape_sf()`](https://cidm-ph.github.io/ggmapinset/reference/shape_sf.md).

## ggmapinset 0.3.0

CRAN release: 2023-04-28

- Replaced the confusing `inset_clip` and `inset_copy` parameters of
  [`geom_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.md)
  with the new `map_base` and `map_inset` parameters.
- Inset frame aesthetics can now be specified consistently.
- A default inset configuration can be passed to
  [`coord_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/coord_sf_inset.md)
  to avoid repeating it for every layer.
- Insets are now based on
  [`stat_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.md)
  which correctly adjusts the coordinate limits and computes some
  inset-related variables for downstream use.
- The new
  [`stat_sf_coordinates_inset()`](https://cidm-ph.github.io/ggmapinset/reference/stat_sf_coordinates_inset.md),
  [`geom_sf_text_inset()`](https://cidm-ph.github.io/ggmapinset/reference/stat_sf_coordinates_inset.md),
  and
  [`geom_sf_label_inset()`](https://cidm-ph.github.io/ggmapinset/reference/stat_sf_coordinates_inset.md)
  labels complete the coverage of sf-related layers.
- Added
  [`transform_to_inset()`](https://cidm-ph.github.io/ggmapinset/reference/transform_to_inset.md)
  helper for applying the inset transformation to arbitrary geometries
  (for extension developers).

## ggmapinset 0.2.3

CRAN release: 2023-02-21

- Initial CRAN release.
