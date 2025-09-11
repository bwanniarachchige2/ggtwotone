GeomCurveDual <- ggplot2::ggproto("GeomCurveDual", ggplot2::Geom,
                                  required_aes = c("x", "y", "xend", "yend"),

                                  default_aes = ggplot2::aes(
                                    colour1 = "white",
                                    colour2 = "black",
                                    linewidth = 1.2,
                                    curvature = 0.3,
                                    angle = 90,
                                    ncp = 5,
                                    alpha = 1
                                  ),

                                  draw_panel = function(data, panel_params, coord, offset = 0.01) {
                                    coords <- coord$transform(data, panel_params)
                                    valid <- complete.cases(coords[, c("x", "y", "xend", "yend")])
                                    coords <- coords[valid, , drop = FALSE]
                                    if (nrow(coords) == 0) return(nullGrob())

                                    if (!"colour1"  %in% names(coords)) coords$colour1  <- "white"
                                    if (!"colour2" %in% names(coords)) coords$colour2 <- "black"

                                    grobs <- vector("list", nrow(coords) * 2)

                                    for (i in seq_len(nrow(coords))) {
                                      row <- coords[i, ]

                                      dx <- row$xend - row$x
                                      dy <- row$yend - row$y
                                      len <- sqrt(dx^2 + dy^2)
                                      if (len == 0) next

                                      # Perpendicular offset
                                      perp_x <- -dy / len
                                      perp_y <-  dx / len

                                      dx_off <- offset * perp_x
                                      dy_off <- offset * perp_y

                                      # Bottom stroke (drawn first)
                                      grobs[[2*i - 1]] <- grid::curveGrob(
                                        x1 = row$x    - dx_off,
                                        y1 = row$y    - dy_off,
                                        x2 = row$xend - dx_off,
                                        y2 = row$yend - dy_off,
                                        curvature = row$curvature,
                                        angle = row$angle,
                                        ncp = row$ncp,
                                        gp = grid::gpar(col = row$colour2, lwd = row$linewidth, alpha = row$alpha)
                                      )

                                      # Top stroke
                                      grobs[[2*i]] <- grid::curveGrob(
                                        x1 = row$x    + dx_off,
                                        y1 = row$y    + dy_off,
                                        x2 = row$xend + dx_off,
                                        y2 = row$yend + dy_off,
                                        curvature = row$curvature,
                                        angle = row$angle,
                                        ncp = row$ncp,
                                        gp = grid::gpar(col = row$colour1, lwd = row$linewidth, alpha = row$alpha)
                                      )
                                    }

                                    grid::grobTree(do.call(grid::gList, grobs))
                                  }
)

#' @title Dual-Stroke Curved Line Annotations
#' @description
#' `geom_curve_dual()` draws a dual-tone curved line using two strokes (light/dark),
#' with slight perpendicular offset to ensure visibility across mixed backgrounds.
#'
#' @inheritParams ggplot2::geom_curve
#' @param base_color Base Color to derive the dual-tone pair from.
#' @param contrast Minimum contrast ratio to aim for (default is 4.5).
#' @param method_contrast Contrast algorithm to use ("WCAG", "APCA", or "auto")
#' @param offset Perpendicular offset to visually separate strokes (default: 0.003).
#' @param curvature Bend of the curve (positive = counter-clockwise).
#' @param angle Angle between the two control points (default: 90).
#' @param ncp Number of control points (default: 5).
#'
#' @rdname geom_curve_dual
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic dual-stroke curve
#' ggplot() +
#'   geom_curve_dual(
#'     aes(x = 1, y = 1, xend = 4, yend = 4),
#'     curvature = 0.4, linewidth = 2
#'   ) +
#'   theme_void() +
#'   theme(panel.background = element_rect(fill = "black"))
#'
#' # Sine wave style dual-stroke curve with points
#' ggplot() +
#'   geom_curve_dual(
#'     aes(x = 2, y = 1, xend = 4, yend = 3),
#'     curvature = 0.3, offset = 0.003, linewidth = 2, base_color = "red"
#'   ) +
#'   geom_point(aes(x = 2, y = 1), colour = "red", size = 3) +
#'   geom_point(aes(x = 4, y = 3), colour = "blue", size = 3) +
#'   theme_dark(base_size = 14)
#'
#' # Curve on a grayscale tile background
#' tile_df <- expand.grid(x = 1:6, y = 1:4)
#' tile_df$fill <- gray.colors(nrow(tile_df), start = 0, end = 1)
#'
#' ggplot() +
#'   geom_tile(data = tile_df, aes(x = x, y = y, fill = fill), width = 1, height = 1) +
#'   geom_curve_dual(
#'     data = data.frame(x = 1, y = 1, xend = 6, yend = 4),
#'     aes(x = x, y = y, xend = xend, yend = yend),
#'     colour1 = "white", colour2 = "black",
#'     curvature = 0.4, offset = 0.003, linewidth = 2
#'   ) +
#'   scale_fill_identity() +
#'   theme_void() +
#'   coord_fixed()
#'
#'
#' @export
geom_curve_dual <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            offset = 0.003, curvature = 0.3, angle = 90,
                            ncp = 5, base_color = NULL,
                            contrast = 4.5,              # used only if base_color is given
                            method_contrast = "WCAG",    # ''
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  if (!is.null(base_color)) {
    cp <- adjust_contrast_pair(
      color    = base_color,
      contrast = contrast,
      method   = method_contrast,
      quiet    = TRUE
    )

    if (is.null(mapping)) {
      mapping <- ggplot2::aes(
        colour1 = !!cp$light,
        colour2 = !!cp$dark
      )
    } else {
      mapping$colour1 <- rlang::quo(!!cp$light)
      mapping$colour2 <- rlang::quo(!!cp$dark)
    }
  }

  layer(
    geom = GeomCurveDual,
    mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(offset = offset, na.rm = na.rm, ...)
  )
}
