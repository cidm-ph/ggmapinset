% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configure_inset.R
\name{configure_inset}
\alias{configure_inset}
\title{Configure transformations underpinning a map inset}
\usage{
configure_inset(
  shape,
  scale = NULL,
  translation = NULL,
  units = "km",
  crs_working = NULL,
  centre = deprecated(),
  radius = deprecated()
)
}
\arguments{
\item{shape}{Inset shape: see \code{\link[=shape_circle]{shape_circle()}}, \code{\link[=shape_rectangle]{shape_rectangle()}},
or \code{\link[=shape_sf]{shape_sf()}}.}

\item{scale}{Zoom scale: values larger than one will make the inset bigger.}

\item{translation}{Translation (shift) of the inset relative to the centre.
This can be an \code{sf::st_point()} or simply a vector of length 2 containing
the x and y offsets respectively. Units are specified by \code{crs_working}.}

\item{units}{Base length unit (e.g. \code{"km"} or \code{"mi"}). Ignored if
\code{crs_working} is provided. See Details for supported values.}

\item{crs_working}{The coordinate reference system to use internally when
applying the transformations. See Details.}

\item{centre, radius}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
Use \code{shape = shape_circle(centre, radius)} instead.}
}
\value{
An inset configuration object of class \code{inset_config}.
}
\description{
The configuration returned by this function will often be passed to the
coordinate system via \code{\link[=coord_sf_inset]{coord_sf_inset()}} so that it is propagated to all
relevant layers.
}
\details{
The default \code{crs_working} uses the equidistant cylindrical coordinate
reference system with the latitude of true scale set to match the latitude of
\code{centre}. This ensures that circular insets will appear circular in most
cases since the projection is not distorted near the centre. The geometries
are converted to this CRS for the inset transformation and constructing the
inset frame, and are converted back to the CRS of \code{centre} at the end.

The default units are kilometres but can be changed with \code{units}
instead of specifying the whole projection. The possible values for
\code{units} are
\href{https://proj.org/operations/conversions/unitconvert.html#distance-units}{those understood by \code{proj}}:
\itemize{
\item \code{"mm"}: millimetre
\item \code{"cm"}: centimetre
\item \code{"m"}: metre
\item \code{"ft"}: foot
\item \code{"us-ft"}: US survey foot
\item \code{"fath"}: fathom
\item \code{"kmi"}: nautical mile
\item \code{"us-ch"}: US survey chain
\item \code{"us-mi"}: US survey mile
\item \code{"km"}: kilometre
\item \code{"ind-ft"}: Indian foot (1937)
\item \code{"ind-yd"}: Indian yard (1937)
\item \code{"mi"}: Statute mile
\item \code{"yd"}: yard
\item \code{"ch"}: chain
\item \code{"link"}: link
\item \code{"dm"}: decimeter
\item \code{"in"}: inch
\item \code{"ind-ch"}: Indian chain
\item \code{"us-in"}: US survey inch
\item \code{"us-yd"}: US survey yard}
}
\examples{
library(sf)

# circular inset with a 2x enlargement
cfg <- configure_inset(
  shape_circle(centre = c(-82, 35), radius = 50),
  scale = 2,
  translation = c(70, -180),
  units = "mi"
)
}
