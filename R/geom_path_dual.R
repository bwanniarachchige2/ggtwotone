GeomPathDual <- ggplot2::ggproto(
  "GeomPathDual", ggplot2::Geom,

  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    colour1 = "black",
    colour2 = "grey80",
    linewidth = ggplot2::from_theme(linewidth),
    linetype = ggplot2::from_theme(linetype),
    alpha = NA
  ),

  draw_key = function(data, params, size) {
    grid::grobTree(
      grid::segmentsGrob(
        x0 = grid::unit(0.1, "npc"),
        x1 = grid::unit(0.9, "npc"),
        y0 = grid::unit(0.65, "npc"),
        y1 = grid::unit(0.65, "npc"),
        gp = grid::gpar(
          col = scales::alpha(data$colour1 %||% "black", data$alpha),
          lwd = (data$linewidth %||% 0.5) * .pt / 2,
          lineend = "round"
        )
      ),
      grid::segmentsGrob(
        x0 = grid::unit(0.1, "npc"),
        x1 = grid::unit(0.9, "npc"),
        y0 = grid::unit(0.35, "npc"),
        y1 = grid::unit(0.35, "npc"),
        gp = grid::gpar(
          col = scales::alpha(data$colour2 %||% "grey80", data$alpha),
          lwd = (data$linewidth %||% 0.5) * .pt / 2,
          lineend = "round"
        )
      )
    )
  },

  draw_panel = function(data, panel_params, coord,
                        lineend = "round",
                        linejoin = "round",
                        na.rm = FALSE) {

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    if (is.null(data) || nrow(data) == 0) {
      return(grid::nullGrob())
    }

    data <- data[stats::complete.cases(data[, c("x", "y")]), , drop = FALSE]

    if (nrow(data) < 2) {
      return(grid::nullGrob())
    }

    if (!("group" %in% names(data))) {
      data$group <- 1
    }

    coords <- coord$transform(data, panel_params)
    groups <- split(coords, coords$group)

    grobs <- lapply(groups, function(df) {
      if ("order" %in% names(df)) {
        df <- df[order(df$order), , drop = FALSE]
      }

      if (nrow(df) < 2) {
        return(grid::nullGrob())
      }

      lwd <- df$linewidth[1] * ggplot2::.pt
      col1 <- scales::alpha(df$colour1[1], df$alpha[1])
      col2 <- scales::alpha(df$colour2[1], df$alpha[1])

      two_colour_path_grob(
        x = df$x,
        y = df$y,
        col1 = col1,
        col2 = col2,
        lwd = lwd,
        lineend = lineend,
        linejoin = linejoin
      )
    })

    grid::grobTree(children = do.call(grid::gList, grobs))
  }
)

#' Draw dual-stroke paths with side-by-side colours
#'
#' Draws a path using two side-by-side strokes with separate colours,
#' improving visibility across mixed or low-contrast backgrounds.
#'
#' @inheritParams ggplot2::geom_path
#' @param colour1 Colour for one side of the path.
#' @param colour2 Colour for the other side of the path.
#' @param linewidth Width of the full dual-stroke path.
#' @param lineend Line end style.
#' @param linejoin Line join style.
#'
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   x = c(1, 2, 3, 4),
#'   y = c(1, 3, 2, 4)
#' )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_path_dual(
#'     colour1 = "#303030",
#'     colour2 = "#E8E8E8",
#'     linewidth = 8
#'   ) +
#'   theme_minimal()
#'
#' library(grid)
#'
#' # simple test data
#' df <- data.frame(
#'   x = seq(0, 10, length.out = 300)
#' )
#' df$y <- sin(df$x) + 0.2 * cos(3 * df$x)
#'
#' # striped background for visibility testing
#' bg <- data.frame(
#'   xmin = seq(0, 9, by = 1),
#'   xmax = seq(1, 10, by = 1),
#'   ymin = -Inf,
#'   ymax = Inf,
#'   fill = gray(seq(0.15, 0.85, length.out = 10))
#' )
#'
#' ggplot() +
#'   geom_rect(
#'     data = bg,
#'     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
#'    inherit.aes = FALSE
#'   ) +
#'   scale_fill_identity() +
#'   geom_path_dual(
#'     data = df,
#'     aes(x = x, y = y),
#'     color = "black",
#'     color2 = "white",
#'     linewidth = 2
#'   ) +
#'  theme_minimal(base_size = 14)
#'
#' @export
geom_path_dual <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           colour1 = NULL, colour2 = NULL,
                           linewidth = NULL,
                           lineend = "round",
                           linejoin = "round",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomPathDual,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      colour1 = colour1,
      colour2 = colour2,
      linewidth = linewidth,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}
