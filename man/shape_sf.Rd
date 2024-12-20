% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inset_shape_sf.R
\name{shape_sf}
\alias{shape_sf}
\title{Arbitrary geometry as insets}
\usage{
shape_sf(geometry)
}
\arguments{
\item{geometry}{A simple features geometry that is either a polygon or
multipolygon, and is valid and simple.}
}
\value{
A shape definition suitable for use with \code{\link[=configure_inset]{configure_inset()}}.
}
\description{
You can use any polygon to define the shape of the inset, including a
feature from your base map layer.
}
\examples{
library(ggplot2)
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
make_demo <- function(...) {
  ggplot(nc) +
    geom_sf(fill = "grey95", colour = "grey85") +
    # For a filled frame, we want to interleave it between the base layer
    # (above this line) and the target layer (below the following line).
    geom_inset_frame(target.aes = list(fill = "white")) +
    geom_sf_inset(map_base = "none", colour = NA) +
    coord_sf_inset(inset = configure_inset(...)) +
    theme_void()
}
shape <- shape_sf(nc[21,])

make_demo(shape, scale = 6, translation = c(-200, -200))
make_demo(shape, scale = 6, translation = c(-100, -100))
make_demo(shape, scale = 6, translation = c(100, 100))
make_demo(shape, scale = 0.5, translation = c(0, 0))
}
\seealso{
\code{\link[=configure_inset]{configure_inset()}}

Other shapes: 
\code{\link{shape_circle}()},
\code{\link{shape_rectangle}()}
}
\concept{shapes}
