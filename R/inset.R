## low-level helpers for operating on inset geometries

# Transforms inputs to crs_working during FUN evaluation then transforms its
# return value back to the original CRS of the first input.
with_crs_working <- function(crs_working, ..., .f) {
  inputs <- rlang::list2(...)
  crs_orig <- sf::st_crs(inputs[[1]])
  inputs_trans <- lapply(inputs, function(x) sf::st_transform(x, crs_working))
  fun <- rlang::as_function(.f)
  result <- do.call(what = fun, args = inputs_trans)
  sf::st_transform(result, crs_orig)
}

# Retains only the inset's viewport. Assumes that the geometry and viewport are in
# working CRS and that x is an sfc, not an sf.
clip_to_viewport <- function(x, viewport) {
  result <- sf::st_intersection(x, viewport)
  retained <- attr(result, "idx")[, 1]
  list(geometry = result, retained = retained)
}

# Clips away the inset's viewport. Assumes that the geometry and viewport are in
# working CRS and that x is an sfc, not an sf.
clip_away_viewport <- function(x, viewport) {
  result <- sf::st_difference(x, viewport)
  retained <- attr(result, "idx")[, 1]
  list(geometry = result, retained = retained)
}

# Return the inset's viewport of the configured shape. It will be in the working
# CRS of the inset configuration.
inset_viewport <- function(inset) {
  UseMethod("inset_viewport")
}

# Applies translation and scale. Assumes that the geometry and centre are in the
# working CRS and that x is an sfc, not an sf.
transform <- function(x, centre, scale = NULL, translation = NULL) {
  crs_working <- sf::st_crs(x)
  result <- x
  if (!is.null(scale)) {
    result <- (result - centre) * scale + centre
    result <- sf::st_set_crs(result, crs_working)
  }
  if (!is.null(translation)) {
    result <- sf::st_set_crs(result + translation, crs_working)
  }
  result
}

# Compute the bounding box of the target part of the inset only
inset_bbox <- function(inset) {
  scale <- inset_scale(inset)
  translation <- inset_translation(inset)

  result <- with_crs_working(
    inset_crs_working(inset),
    inset_centre(inset),
    .f = function(centre) {
      viewport <- inset_viewport(inset)
      transform(viewport, centre, scale = scale, translation = translation)
    }
  )
  sf::st_bbox(result)
}
