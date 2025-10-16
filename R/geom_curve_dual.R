GeomCurveDual <- ggplot2::ggproto("GeomCurveDual", ggplot2::Geom,
                                  required_aes = c("x", "y", "xend", "yend"),

                                  default_aes = ggplot2::aes(
                                    colour1 = "white",
                                    colour2 = "black",
                                    linewidth = 1.2,
                                    curvature = 0.3,
                                    angle = 90,
                                    ncp = 21,
                                    alpha = 1,
                                    lineend = "round",
                                    linejoin = "round",
                                    linemitre = 10
                                  ),

                                  draw_panel = function(data, panel_params, coord,
                                                        curvature = 0.3, angle = 90, ncp = 21
                                                        ) {
                                    coords <- coord$transform(data, panel_params)
                                    valid <- complete.cases(coords[, c("x", "y", "xend", "yend")])
                                    coords <- coords[valid, , drop = FALSE]
                                    if (nrow(coords) == 0) return(nullGrob())

                                    if (!"colour1"  %in% names(coords)) coords$colour1  <- "white"
                                    if (!"colour2" %in% names(coords)) coords$colour2 <- "black"

                                    # fallback to params if aesthetics missing/NA
                                    if (!"curvature" %in% names(coords) || all(is.na(coords$curvature))) coords$curvature <- curvature
                                    if (!"angle"     %in% names(coords) || all(is.na(coords$angle)))     coords$angle     <- angle
                                    if (!"ncp"       %in% names(coords) || all(is.na(coords$ncp)))       coords$ncp       <- ncp

                                    # control points like ggplot2::GeomCurve
                                    control_pts <- function(x1, y1, x2, y2, curvature, angle) {
                                      dx <- x2 - x1; dy <- y2 - y1
                                      L  <- sqrt(dx^2 + dy^2)
                                      if (L == 0) return(c((x1 + x2)/2, (y1 + y2)/2))

                                      # unit tangent (t) and unit normal (n)
                                      tx <- dx / L; ty <- dy / L
                                      nx <- -ty;     ny <- tx

                                      # angle is measured from tangent toward normal
                                      theta <- angle * (pi/180)
                                      ux <- cos(theta) * tx + sin(theta) * nx
                                      uy <- cos(theta) * ty + sin(theta) * ny

                                      # midpoint + scaled offset; scale by span so curvature is length-aware
                                      xm <- (x1 + x2)/2; ym <- (y1 + y2)/2
                                      d  <- curvature * L

                                      c(xm + d * ux, ym + d * uy)
                                    }

                                    # quadratic Bezier sampler (n points)
                                    bezier_q <- function(x1, y1, cx, cy, x2, y2, n) {
                                      t <- seq(0, 1, length.out = n)
                                      bx <- (1 - t)^2 * x1 + 2*(1 - t)*t*cx + t^2 * x2
                                      by <- (1 - t)^2 * y1 + 2*(1 - t)*t*cy + t^2 * y2
                                      list(x = bx, y = by)
                                    }

                                    # polyline normals (central/forward/backward differences)
                                    path_normals <- function(x, y) {
                                      nx <- numeric(length(x)); ny <- numeric(length(y))
                                      # tangent by finite diff
                                      dx <- c(diff(x), 0); dy <- c(diff(y), 0)
                                      dx[-1] <- (dx[-1] + dx[-length(dx)])/2
                                      dy[-1] <- (dy[-1] + dy[-length(dy)])/2
                                      # perpendicular + normalize
                                      nx <- -dy; ny <- dx
                                      len <- sqrt(nx^2 + ny^2)
                                      len[len == 0] <- 1
                                      nx/len -> nx; ny/len -> ny
                                      list(nx = nx, ny = ny)
                                    }

                                    grobs <- vector("list", nrow(coords) * 2)
                                    gi <- 1L

                                    for (i in seq_len(nrow(coords))) {
                                      row <- coords[i, ]
                                      if (row$ncp < 3) row$ncp <- 3

                                      cxy <- control_pts(row$x, row$y, row$xend, row$yend, row$curvature, row$angle)
                                      bz  <- bezier_q(row$x, row$y, cxy[1], cxy[2], row$xend, row$yend, n = row$ncp)
                                      nrm <- path_normals(bz$x, bz$y)

                                      # Convert ggplot linewidth (mm) → grid points
                                      lwd_pt      <- row$linewidth * ggplot2::.pt
                                      off_side_pt <- lwd_pt / 5
                                      eps_pt      <- 0.25
                                      # per-vertex offsets in *points*
                                      ox <- grid::unit(nrm$nx * off_side_pt, "pt")
                                      oy <- grid::unit(nrm$ny * off_side_pt, "pt")

                                      # base positions in native units
                                      ux <- grid::unit(bz$x, "native")
                                      uy <- grid::unit(bz$y, "native")

                                      # dark/bottom stroke first
                                      grobs[[gi]] <- grid::polylineGrob(
                                        x = ux - ox, y = uy - oy,
                                        gp = grid::gpar(
                                          col = row$colour2,
                                          lwd = (lwd_pt / 2) + eps_pt,   # half width (+epsilon)
                                          alpha = row$alpha,
                                          lineend = row$lineend,
                                          linejoin = row$linejoin,
                                          linemitre = row$linemitre
                                        )
                                      ); gi <- gi + 1L

                                      # light/top stroke
                                      grobs[[gi]] <- grid::polylineGrob(
                                        x = ux + ox, y = uy + oy,
                                        gp = grid::gpar(
                                          col = row$colour1,
                                          lwd = (lwd_pt / 2),            # half width
                                          alpha = row$alpha,
                                          lineend = row$lineend,
                                          linejoin = row$linejoin,
                                          linemitre = row$linemitre
                                        )
                                      ); gi <- gi + 1L
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
#' b <- ggplot(mtcars, aes(wt, mpg)) + geom_point(size = 2) + theme_dark()
#' df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
#' b + geom_curve_dual(
#'     data = df,
#'     mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
#'     curvature = -0.2,
#'     linewidth = 2, base_color = "green"
#'   )
#'
#'
#' # Sine wave style dual-stroke curve with points
#' ggplot() +
#'   geom_curve_dual(
#'     aes(x = 2, y = 1, xend = 4, yend = 3),
#'     curvature = 0.3,
#'     linewidth = 2, base_color = "red"
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
#'     curvature = 0.4,
#'     linewidth = 2
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
                            curvature = NULL, angle = NULL,
                            ncp = NULL, base_color = NULL,
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

  # make sure mapping exists
  if (is.null(mapping)) mapping <- ggplot2::aes()

  # inject constants so draw_panel() can read them from `data`
  if (!is.null(curvature) && is.null(mapping$curvature))
    mapping$curvature <- rlang::quo(!!curvature)
  if (!is.null(angle) && is.null(mapping$angle))
    mapping$angle <- rlang::quo(!!angle)
  if (!is.null(ncp) && is.null(mapping$ncp))
    mapping$ncp <- rlang::quo(!!ncp)

  layer(
    geom = GeomCurveDual,
    mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
                  na.rm = na.rm, ...)
  )
}
