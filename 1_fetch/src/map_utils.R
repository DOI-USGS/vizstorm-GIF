
bbox_to_polygon <- function(bbox, bbox_crs = "+init=epsg:4326", return_crs = NULL) {
  bbox_poly <- sf::st_sfc(sf::st_polygon(list(
    matrix(bbox[c(1, 2, 1, 4, 3, 4, 3, 2, 1, 2)], ncol = 2, byrow = TRUE)),
    dim = "XY"), crs = bbox_crs)
  if (!is.null(return_crs)) {
    sf::st_transform(bbox_poly, return_crs)
  }
  else {
    bbox_poly
  }
}



as_view_polygon <- function(object) {

  # the projected polygon specified by the user
  bbox_projected <- bbox_to_polygon(bbox = object$bbox, return_crs = object$projection)

  poly_bbox <- sf::st_bbox(bbox_projected)

  aspect <- object$aspect
  bbox_aspect <- diff(poly_bbox[c(1,3)]) / diff(poly_bbox[c(2,4)])
  if (bbox_aspect > aspect){ # flatter than it should be
    new_y_diff <- diff(poly_bbox[c(1,3)]) / aspect
    y_buffer <- new_y_diff - diff(poly_bbox[c(2,4)])
    poly_bbox[2] <- poly_bbox[2] - y_buffer / 2
    poly_bbox[4] <- poly_bbox[4] + y_buffer / 2

  } else { # taller than it should be
    new_x_diff <- diff(poly_bbox[c(2,4)]) * aspect
    x_buffer <- new_x_diff - diff(poly_bbox[c(1,3)])
    poly_bbox[1] <- poly_bbox[1] - x_buffer / 2
    poly_bbox[3] <- poly_bbox[3] + x_buffer / 2
  }

  view_poly <- bbox_to_polygon(poly_bbox, bbox_crs = st_crs(bbox_projected))
  return(view_poly)
}
