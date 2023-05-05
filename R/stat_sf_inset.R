#' @export
#' @rdname geom_sf_inset
stat_sf_inset <- function(mapping = ggplot2::aes(), data = NULL,
                          geom = "sf_inset", position = "identity",
                          ...,
                          inset = NA,
                          na.rm = TRUE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer_sf(
    mapping = mapping,
    data = data,
    stat = StatSfInset,
    geom = geom,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = rlang::list2(
      inset = inset,
      na.rm = na.rm,
      ...
    ))
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname geom_sf_inset
StatSfInset <- ggplot2::ggproto("StatSfInset", ggplot2::StatSf,
  compute_panel = function(data, scales, coord, inset = NA) {
    data <- ggplot2::StatSf$compute_panel(data, scales, coord)

    inset <- get_inset_config(inset, coord)

    # we also need to let the extend the coord boundaries and range to include
    # the transformed inset
    if (!is.null(inset) && inherits(coord, "CoordSf")) {
      # if (sf::st_crs(inset) != sf::st_crs(data)) {
      #   cli::cli_warn(c("Inset coordinate reference system does not match data",
      #                    "i" = "The {.field centre} of the inset uses a different CRS to the data; the inset might be drawn incorrectly"))
      # }

      bbox <- inset_bbox(inset)

      coord$record_bbox(
        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
      )

      bbox_trans <- ggplot2::sf_transform_xy(
        list(
          x = c(rep(0.5 * (bbox[["xmin"]] + bbox[["xmax"]]), 2), bbox[["xmin"]], bbox[["xmax"]]),
          y = c(bbox[["ymin"]], bbox[["ymax"]], rep(0.5 * (bbox[["ymin"]] + bbox[["ymax"]]), 2))
        ),
        sf::st_crs(bbox),
        sf::st_crs(data)
      )

      data$xmin <- min(data$xmin, bbox_trans$x)
      data$xmax <- max(data$xmax, bbox_trans$x)
      data$ymin <- min(data$ymin, bbox_trans$y)
      data$ymax <- max(data$ymax, bbox_trans$y)
    }

    data
  }
)

# Compute the bounding box of the target part of the inset only
inset_bbox <- function(inset) {
  scale <- inset_scale(inset)
  translation <- inset_translation(inset)
  radius <- inset_radius(inset)
  result <- with_crs_working(
    inset_crs_working(inset),
    inset_centre(inset),
    .f = function(centre) {
      viewport <- circular_viewport(centre, radius)
      transform(viewport, centre, scale = scale, translation = translation)
    })
  sf::st_bbox(result)
}
