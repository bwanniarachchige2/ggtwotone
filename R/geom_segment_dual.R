# Grob for drawing a line segment in two (contrasting) colours side by side
two_colour_segment_grob <- function(x0, y0, x1, y1, col1, col2, lwd, lineend, arrow, arrow.fill) {
  dx <- grid::convertWidth(grid::unit(x1 - x0, "npc"), "pt", valueOnly = TRUE)
  dy <- grid::convertHeight(grid::unit(y1 - y0, "npc"), "pt", valueOnly = TRUE)

  len <- sqrt(dx^2 + dy^2)

  if (!is.finite(len) || len == 0) {
    return(grid::gTree(children = grid::gList()))
  }

  perp_x <- -dy / len
  perp_y <-  dx / len

  offset_amt <- lwd / 4
  offset_x <- perp_x * offset_amt
  offset_y <- perp_y * offset_amt

  right_line <- grid::segmentsGrob(
    x0 = grid::unit(x0, "npc") - grid::unit(offset_x, "pt"),
    y0 = grid::unit(y0, "npc") - grid::unit(offset_y, "pt"),
    x1 = grid::unit(x1, "npc") - grid::unit(offset_x, "pt"),
    y1 = grid::unit(y1, "npc") - grid::unit(offset_y, "pt"),
    gp = grid::gpar(
      col = col1,
      lwd = lwd / 2,
      lineend = lineend,
      fill = if (is.null(arrow.fill)) col1 else arrow.fill
    ),
    arrow = arrow
  )

  left_line <- grid::segmentsGrob(
    x0 = grid::unit(x0, "npc") + grid::unit(offset_x, "pt"),
    y0 = grid::unit(y0, "npc") + grid::unit(offset_y, "pt"),
    x1 = grid::unit(x1, "npc") + grid::unit(offset_x, "pt"),
    y1 = grid::unit(y1, "npc") + grid::unit(offset_y, "pt"),
    gp = grid::gpar(
      col = col2,
      lwd = lwd / 2,
      lineend = lineend,
      fill = if (is.null(arrow.fill)) col2 else arrow.fill
    ),
    arrow = arrow
  )

  grid::gTree(children = grid::gList(right_line, left_line))
}

GeomSegmentDual <- ggplot2::ggproto(
  "GeomSegmentDual", ggplot2::Geom,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = ggplot2::aes(
    linewidth = ggplot2::from_theme(linewidth),
    linetype = ggplot2::from_theme(linetype),
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_path,

  draw_panel = function(data, panel_params, coord,
                        lineend = "butt", arrow = NULL, arrow.fill = NULL,
                        colour1 = NULL, colour2 = NULL) {

    coords <- coord$transform(data, panel_params)
    n <- nrow(coords)

    default_cols <- adjust_contrast_pair("#303030")
    default_colour1 <- default_cols$dark
    default_colour2 <- default_cols$light

    col1 <- if (!is.null(colour1)) {
      rep_len(colour1, n)
    } else if ("colour1" %in% names(coords)) {
      coords$colour1
    } else {
      rep_len(default_colour1, n)
    }

    col2 <- if (!is.null(colour2)) {
      rep_len(colour2, n)
    } else if ("colour2" %in% names(coords)) {
      coords$colour2
    } else {
      rep_len(default_colour2, n)
    }

    alpha_vals <- coords$alpha
    alpha_vals[is.na(alpha_vals)] <- 1

    grobs <- lapply(seq_len(n), function(i) {
      row <- coords[i, , drop = FALSE]
      lwd <- row$linewidth * ggplot2::.pt

      two_colour_segment_grob(
        x0 = row$x, y0 = row$y,
        x1 = row$xend, y1 = row$yend,
        col1 = scales::alpha(col1[i], alpha_vals[i]),
        col2 = scales::alpha(col2[i], alpha_vals[i]),
        lwd = lwd,
        lineend = lineend,
        arrow = arrow,
        arrow.fill = arrow.fill
      )
    })

    grid::grobTree(children = do.call(grid::gList, grobs))
  }
)

#' Dual-Stroke Line Segments with Side-by-Side Offset
#'
#' Draws two side-by-side line segments with separate colours for improved
#' visibility on varied backgrounds.
#'
#' @title Dual-Stroke Line Segments with Side-by-Side Offset
#' @description Draws two side-by-side line segments with separate colours for
#' improved visibility on varied backgrounds.
#'
#' @inheritParams ggplot2::geom_segment
#' @param colour1 Colour for one side of the dual stroke.
#' @param colour2 Colour for the other side of the dual stroke.
#' @param linewidth Width of the total dual stroke (in mm).
#' @param aspect_ratio Aspect ratio hint (currently unused by the grob logic but reserved for future layout tuning).
#'
#'
#' @examples
#' # Simple black background test
#' ggplot(data.frame(x = 1, xend = 2, y = 1, yend = 2),
#'        aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_segment_dual(colour1 = "white", colour2 = "black", linewidth = 2) +
#'   theme_void() +
#'   theme(panel.background = element_rect(fill = "gray20"))
#'
#' # Dual-stroke diagonal lines crossing contrasting backgrounds
#' bg <- data.frame(
#'   xmin = c(0, 5),
#'   xmax = c(5, 10),
#'   ymin = 0,
#'   ymax = 5,
#'   fill = c("black", "white")
#' )
#'
#' line_data <- data.frame(
#'   x = c(1, 9),
#'   y = c(1, 1),
#'   xend = c(9, 1),
#'   yend = c(4, 4),
#'   colour1 = c("#D9D9D9", "#D9D9D9"),  # light stroke
#'   colour2 = c("#333333", "#333333")   # dark stroke
#' )
#'
#' ggplot() +
#'   geom_rect(data = bg,
#'   aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
#'   inherit.aes = FALSE) +
#'   scale_fill_identity() +
#'   geom_segment_dual(
#'     data = line_data,
#'     aes(x = x, y = y, xend = xend, yend = yend),
#'     colour1 = line_data$colour1,
#'     colour2 = line_data$colour2,
#'     linewidth = 1,
#'     inherit.aes = FALSE
#'   ) +
#'   theme_void() +
#'   coord_fixed() +
#'   ggtitle("Two Diagonal Dual-Stroke Lines in Opposite Directions")
#'
#' # Multiple dual-stroke segments with arrowheads and grouping
#' df <- data.frame(
#'   x = c(1, 2, 3),
#'   xend = c(2, 3, 4),
#'   y = c(1, 2, 1),
#'   yend = c(2, 1, 2),
#'   colour1 = rep("white", 3),
#'   colour2 = rep("black", 3),
#'   group = factor(c("A", "B", "C"))
#' )
#'
#' ggplot(df) +
#'   geom_segment_dual(
#'     aes(x = x, y = y, xend = xend, yend = yend, group = group),
#'     colour1 = df$colour1,
#'     colour2 = df$colour2,
#'     linewidth = 1,
#'     arrow = arrow(length = unit(0.15, "inches"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_dark()
#' @export
geom_segment_dual <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              colour1 = NULL, colour2 = NULL, linewidth = NULL,
                              lineend = "butt", aspect_ratio = 1,
                              ..., arrow = NULL, arrow.fill = NULL,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomSegmentDual,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linewidth = linewidth,
      colour1 = colour1,
      colour2 = colour2,
      na.rm = na.rm,
      ...
    )
  )
}
