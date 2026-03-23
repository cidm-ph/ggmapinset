# Convert an object to a point in a suitable format

This is an internal helper used by the shape definition functions to
process their `centre` parameters, where present. It is not intended to
be used directly. Instead, it allows extensions to integrate with the
shape functions so that they can accept new definitions.

## Usage

``` r
coerce_centre(centre, ...)
```

## Arguments

- centre:

  Object defining the centre point.

- ...:

  Unused in the default implementation.

## Value

A `sfc` object containins a single
[`sf::st_point`](https://r-spatial.github.io/sf/reference/st.html)
feature and with an appropriate CRS set.
