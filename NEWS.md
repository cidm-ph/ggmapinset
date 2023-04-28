# ggmapinset 0.3.0

* Replaced the confusing `inset_clip` and `inset_copy` parameters of
  `geom_sf_inset()` with the new `map_base` and `map_inset` parameters.
* Inset frame aesthetics can now be specified consistently.
* A default inset configuration can be passed to `coord_sf_inset()` to avoid
  repeating it for every layer.
* Insets are now based on `stat_sf_inset()` which correctly adjusts the
  coordinate limits and computes some inset-related variables for downstream use.
* The new `stat_sf_coordinates_inset()`, `stat_sf_text_inset()`, and
  `stat_sf_label_inset()` labels complete the coverage of sf-related layers.
* Added `transform_to_inset()` helper for applying the inset transformation to
  arbitrary geometries (for extension developers).

# ggmapinset 0.2.3

* Initial CRAN release.
