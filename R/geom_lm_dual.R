#' Dual-Tone Regression Line with Contrast-Aware Strokes
#'
#' Draws a regression line with perceptually distinct dual-stroke coloring for improved visibility.
#'
#' @param data A data frame containing the variables.
#' @param mapping Aesthetic mapping, must include `x` and `y`.
#' @param method Regression method to use (default is "lm").
#' @param formula Model formula (default is `y ~ x`).
#' @param base_color Base color to derive the dual-tone pair from.
#' @param contrast Minimum contrast ratio to aim for (default is 4.5).
#' @param method_contrast Contrast algorithm to use ("WCAG", "APCA", or "auto").
#' @param linewidth Total visual line thickness in mm (both side strokes together).
#' @param show.legend Whether to show legend.
#' @param ... Additional parameters passed to `geom_segment_dual()`.
#'
#' @return A `ggplot2` layer containing the dual-stroke regression line.
#'
#' @examples
#' library(ggplot2)
#'
#' # Simple test with linear trend
#' set.seed(42)
#' df <- data.frame(x = 1:100, y = 0.5 * (1:100) + rnorm(100))
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_lm_dual(data = df, mapping = aes(x = x, y = y)) +
#'   theme_minimal()
#'
#' # Over grayscale tiles
#' x <- seq(1, 11, length.out = 100)
#' y <- 0.5 * x + rnorm(100, 0, 0.3)
#' df1 <- data.frame(x = x, y = y)
#'
#' # Tile fill definitions
#' fill_colors <- data.frame(
#'   x = 1:11,
#'   fill = c("#000000", "#1b1b1b", "#444444", "#777777", "#aaaaaa",
#'            "#dddddd", "#D5D5D5", "#E5E5E5", "#F5F5F5", "#FAFAFA", "#FFFFFF")
#' )
#'
#' # Expand tile grid and join with fill colors
#' tiles <- expand.grid(x = 1:11, y = seq(0, 1, length.out = 100)) |>
#'   merge(fill_colors, by = "x")
#'
#' ggplot() +
#'   geom_tile(
#'     data = tiles, aes(x = x, y = y, fill = fill),
#'     width = 1, height = 10
#'   ) +
#'   scale_fill_identity() +
#'   geom_point(
#'     data = df1, aes(x = x, y = y),
#'     colour = "purple", size = 2
#'   ) +
#'   ## Uncomment to use points with frames:
#'   # geom_point(
#'   #   data = df1, aes(x = x, y = y),
#'   #   shape = 21, colour = "white", fill = "black", size = 3
#'   # ) +
#'   geom_lm_dual(
#'     data = df1, mapping = aes(x = x, y = y),
#'     linewidth = 2
#'   ) +
#'   coord_fixed() +
#'   theme_minimal()
#' @export
geom_lm_dual <- function(data, mapping, method = "lm", formula = y ~ x,
                         base_color = "#777777", contrast = 4.5,
                         method_contrast = "WCAG", ...,
                         linewidth = 1, show.legend = NA) {

  x_var <- rlang::as_name(mapping$x)
  y_var <- rlang::as_name(mapping$y)

  model <- stats::lm(formula, data = data)
  x_range <- range(data[[x_var]], na.rm = TRUE)
  new_data <- data.frame(x = x_range)
  names(new_data) <- x_var
  y_pred <- predict(model, newdata = new_data)

  cp <- adjust_contrast_pair(base_color, contrast = contrast, method = method_contrast)

  reg_segment <- data.frame(
    x = x_range[1],
    y = y_pred[1],
    xend = x_range[2],
    yend = y_pred[2]
  )

  geom_segment_dual(
    data = reg_segment,
    mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
    colour1 = cp$light,
    colour2 = cp$dark,
    linewidth = linewidth,
    inherit.aes = FALSE,
    show.legend = show.legend,
    ...
  )
}
