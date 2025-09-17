GeomPathDualSide <- ggplot2::ggproto(
  "GeomPathDualSide", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes  = ggplot2::aes(
    colour1 = "white",
    colour2 = "black",
    linewidth = 1.2,
    alpha = 1,
    lineend = "round",
    linejoin = "round",
    linemitre = 10
  ),
  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    if (nrow(coords) < 2) return(grid::nullGrob())

    # split by group so each path renders separately
    groups <- split(coords, coords$group)
    grobs  <- vector("list", length(groups) * 2L)
    gi <- 1L

    for (g in groups) {
      if (nrow(g) < 2) next

      # finite-diff tangents -> normals
      dx <- c(diff(g$x), 0); dy <- c(diff(g$y), 0)
      dx[-1] <- (dx[-1] + dx[-length(dx)])/2
      dy[-1] <- (dy[-1] + dy[-length(dy)])/2
      nx <- -dy; ny <- dx
      len <- sqrt(nx^2 + ny^2); len[len == 0] <- 1
      nx <- nx/len; ny <- ny/len

      lwd_pt      <- g$linewidth[1] * ggplot2::.pt
      off_side_pt <- lwd_pt / 5
      eps_pt      <- 0.25

      ux <- grid::unit(g$x, "native")
      uy <- grid::unit(g$y, "native")
      ox <- grid::unit(nx * off_side_pt, "pt")
      oy <- grid::unit(ny * off_side_pt, "pt")

      # bottom (dark) first, a hair thicker
      grobs[[gi]] <- grid::polylineGrob(
        x = ux - ox, y = uy - oy,
        gp = grid::gpar(
          col = g$colour2[1],
          lwd = (lwd_pt / 2) + eps_pt,
          alpha = g$alpha[1],
          lineend = g$lineend[1],
          linejoin = g$linejoin[1],
          linemitre = g$linemitre[1]
        )
      ); gi <- gi + 1L

      # top (light)
      grobs[[gi]] <- grid::polylineGrob(
        x = ux + ox, y = uy + oy,
        gp = grid::gpar(
          col = g$colour1[1],
          lwd = (lwd_pt / 2),
          alpha = g$alpha[1],
          lineend = g$lineend[1],
          linejoin = g$linejoin[1],
          linemitre = g$linemitre[1]
        )
      ); gi <- gi + 1L
    }

    grid::grobTree(do.call(grid::gList, grobs))
  }
)

#' Dual-Tone Curved Function Lines
#'
#' Draws a function (e.g., density or mathematical curve) using perceptually offset dual-stroke curved line segments.
#'
#' @param fun A function to evaluate (e.g., `dnorm`, `dt`).
#' @param xlim Range of x-values to evaluate over (numeric vector of length 2).
#' @param n Number of segments to compute (default: 201).
#' @param curvature,angle,ncp Passed to underlying `geom_curve_dual` segments.
#' @param colour1,colour2 Fixed top/bottom stroke colours. If only colour1 given,
#'   colour2 is derived for contrast. (Aliases color1/color2 also accepted.)
#' @param base_color Optional base color to derive a contrast pair (overrides colour1/colour2 if supplied).
#' @param color1,color2 U.S.-spelling aliases for `colour1`/`colour2`. Identical
#'   in effect; prefer `colour1`/`colour2` in code examples.
#' @param contrast,method_contrast Passed to adjust_contrast_pair() when deriving colors.
#' @param linewidth Stroke width for the top line.
#' @param args List of arguments passed to `fun` (for example, list(df = 1) for `dt`).
#' @param smooth Use smooth dual-stroke curves (`geom_path`) instead of segmented curves (`geom_curve_dual`). Default is TRUE.
#' @param alpha Overall opacity for both strokes (0–1).
#' @param ... Additional arguments passed to `geom_curve_dual()`.
#'
#' @return A `ggplot2` layer with curved segments.
#'
#' @rdname geom_curve_dual_function
#'
#' @examples
#' library(ggplot2)
#'
#' base <- ggplot() + xlim(-2.05,2.05)
#' base +
#'   geom_curve_dual_function(
#'   fun = function(x) 0.5 * exp(-abs(x)),
#'   xlim = c(-2, 2),
#'   color1 = "#EEEEEE",
#'   color2 = "#222222",
#'   linewidth = 1,
#'   smooth = TRUE
#'   ) +
#'   theme_dark()
#'
#'
#' ggplot() +
#'   geom_curve_dual_function(
#'     fun = dnorm,
#'     xlim = c(-5, 5),
#'     base_color = "green",
#'     linewidth = 1,
#'     smooth = TRUE
#'   ) +
#'   geom_curve_dual_function(
#'     fun = dt,
#'     args = list(df = 1),
#'     xlim = c(-5, 5),
#'     base_color = "brown",
#'     linewidth = 1,
#'     smooth = TRUE
#'   ) +
#'   theme_dark()
#'
#' @export
geom_curve_dual_function <- function(fun,
                                     xlim = c(-3, 3),
                                     n = 701,
                                     curvature = 0,     # used only in non-smooth branch
                                     angle = 90,        # ''
                                     ncp = 5,           # ''
                                     colour1 = NULL,
                                     colour2 = NULL,
                                     base_color = NULL,
                                     contrast = 4.5,
                                     method_contrast = "WCAG",
                                     linewidth = 1.2,
                                     args = list(),
                                     smooth = TRUE,
                                     # aliases for convenience
                                     color1 = NULL,
                                     color2 = NULL,
                                     alpha = 1,
                                     ...) {
  if (!is.null(color1) && is.null(colour1)) colour1 <- color1
  if (!is.null(color2) && is.null(colour2)) colour2 <- color2

  # colours: base_color wins; else derive colour2 from colour1; else defaults
  if (!is.null(base_color)) {
    cp <- adjust_contrast_pair(color = base_color, contrast = contrast,
                               method = method_contrast, quiet = TRUE)
    colour1 <- cp$light; colour2 <- cp$dark
  } else if (!is.null(colour1) && is.null(colour2)) {
    cp <- adjust_contrast_pair(color = colour1, contrast = contrast,
                               method = method_contrast, quiet = TRUE)
    colour2 <- cp$dark
  } else {
    if (is.null(colour1)) colour1 <- "white"
    if (is.null(colour2)) colour2 <- "black"
  }

  # sample & evaluate
  x_vals <- seq(xlim[1], xlim[2], length.out = if (smooth) max(1001L, n) else n)
  y_vals <- tryCatch({
    do.call(fun, c(list(x_vals), args))
  }, error = function(e) {
    warning("Could not evaluate function with given args: ", e$message)
    rep(NA_real_, length(x_vals))
  })
  keep <- is.finite(x_vals) & is.finite(y_vals)
  x_vals <- x_vals[keep]; y_vals <- y_vals[keep]
  if (length(x_vals) < 2L) return(ggplot2::geom_blank())

  if (isTRUE(smooth)) {
    # smooth, pt-aware side-by-side (no user offset)
    df <- data.frame(x = x_vals, y = y_vals, group = 1L)

    return(ggplot2::layer(
      geom = GeomPathDualSide,
      mapping = ggplot2::aes(x = x, y = y),
      data = df,
      stat = "identity",
      position = "identity",
      inherit.aes = FALSE,
      params = list(
        linewidth = linewidth,
        colour1   = colour1,
        colour2   = colour2,
        alpha     = alpha
      )
    ))
  }

  # non-smooth: tiny segments via your GeomCurveDual
  data <- data.frame(
    x    = head(x_vals, -1L),
    y    = head(y_vals, -1L),
    xend = tail(x_vals, -1L),
    yend = tail(y_vals, -1L)
  )
  data <- tidyr::drop_na(data)

  geom_curve_dual(
    data      = data,
    mapping   = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
    curvature = curvature,
    angle     = angle,
    ncp       = max(3L, ncp),
    colour1   = colour1,
    colour2   = colour2,
    linewidth = linewidth,
    alpha     = alpha,
    ...
  )
}
