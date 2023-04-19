# ggmapinset 0.3.0

* Added `transform_to_inset()` helper for applying the inset transformation to
  arbitrary geometries.
* Replaced the confusing `inset_clip` and `inset_copy` parameters of
  `geom_sf_inset()` with the new `map_base` and `map_inset` parameters.
* Inset frame aesthetics can now be specified consistently.
* A default inset configuration can be passed to `coord_sf_inset()` to avoid
  repeating it for every layer.

# ggmapinset 0.2.3

* Initial CRAN release.
