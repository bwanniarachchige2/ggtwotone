#' @keywords internal
safe_as_hex <- function(col) {
  tryCatch({
    rgb <- grDevices::col2rgb(col) / 255
    rgb_to_hex <- rgb(rgb[1,], rgb[2,], rgb[3,])
    return(rgb_to_hex)
  }, error = function(e) {
    rep(NA_character_, length(col))
  })
}

get_contrast <- function(fg, bg, method = c("WCAG", "APCA")) {
  method <- match.arg(method)
  tryCatch({
    cr <- colorspace::contrast_ratio(fg, bg, algorithm = method)
    if (method == "APCA") {
      if (is.matrix(cr)) abs(cr[, "normal"]) else abs(cr["normal"])
    } else {
      cr
    }
  }, error = function(e) NA)
}

compute_best_text_color <- function(bg_color, method = "APCA", light = "#eeeeee", dark = "#111111") {
  bg_color <- safe_as_hex(bg_color)
  sapply(bg_color, function(bg) {
    c_light <- get_contrast(light, bg, method)
    c_dark  <- get_contrast(dark, bg, method)
    if (is.na(c_light) && is.na(c_dark)) return(dark)
    if (c_light > c_dark) light else dark
  })
}

two_colour_path_grob <- function(x, y, col1, col2, lwd,
                                 lineend = "round", linejoin = "round") {
  n <- length(x)

  if (n < 2) {
    return(grid::gTree(children = grid::gList()))
  }

  dx_pt <- numeric(n - 1)
  dy_pt <- numeric(n - 1)

  for (i in seq_len(n - 1)) {
    dx_pt[i] <- grid::convertWidth(
      grid::unit(x[i + 1] - x[i], "npc"), "pt", valueOnly = TRUE
    )
    dy_pt[i] <- grid::convertHeight(
      grid::unit(y[i + 1] - y[i], "npc"), "pt", valueOnly = TRUE
    )
  }

  seg_len <- sqrt(dx_pt^2 + dy_pt^2)
  keep_seg <- is.finite(seg_len) & seg_len > 0

  if (!any(keep_seg)) {
    return(grid::gTree(children = grid::gList()))
  }

  # unit normals for each segment
  nx_seg <- rep(NA_real_, n - 1)
  ny_seg <- rep(NA_real_, n - 1)

  nx_seg[keep_seg] <- -dy_pt[keep_seg] / seg_len[keep_seg]
  ny_seg[keep_seg] <-  dx_pt[keep_seg] / seg_len[keep_seg]

  # desired half-offset in points
  d <- 0.65 * lwd / 4

  offset_x_pt <- numeric(n)
  offset_y_pt <- numeric(n)

  first_ok <- which(keep_seg)[1]
  last_ok  <- tail(which(keep_seg), 1)

  # endpoints: just use segment normal
  offset_x_pt[1] <- nx_seg[first_ok] * d
  offset_y_pt[1] <- ny_seg[first_ok] * d
  offset_x_pt[n] <- nx_seg[last_ok] * d
  offset_y_pt[n] <- ny_seg[last_ok] * d

  if (n > 2) {
    for (i in 2:(n - 1)) {
      left_ok  <- keep_seg[i - 1]
      right_ok <- keep_seg[i]

      if (left_ok && right_ok) {
        # averaged normal direction
        bx <- nx_seg[i - 1] + nx_seg[i]
        by <- ny_seg[i - 1] + ny_seg[i]
        b_len <- sqrt(bx^2 + by^2)

        if (is.finite(b_len) && b_len > 0) {
          bx <- bx / b_len
          by <- by / b_len

          # miter correction: preserve constant perpendicular offset
          denom <- bx * nx_seg[i - 1] + by * ny_seg[i - 1]

          if (is.finite(denom) && abs(denom) > 1e-6) {
            miter_len <- d / denom

            # optional cap to avoid huge spikes at sharp angles
            miter_len <- min(miter_len, 4 * d)

            offset_x_pt[i] <- bx * miter_len
            offset_y_pt[i] <- by * miter_len
          } else {
            offset_x_pt[i] <- bx * d
            offset_y_pt[i] <- by * d
          }
        } else {
          offset_x_pt[i] <- nx_seg[i] * d
          offset_y_pt[i] <- ny_seg[i] * d
        }
      } else if (left_ok) {
        offset_x_pt[i] <- nx_seg[i - 1] * d
        offset_y_pt[i] <- ny_seg[i - 1] * d
      } else if (right_ok) {
        offset_x_pt[i] <- nx_seg[i] * d
        offset_y_pt[i] <- ny_seg[i] * d
      } else {
        offset_x_pt[i] <- 0
        offset_y_pt[i] <- 0
      }
    }
  }

  x_right <- grid::unit(x, "npc") - grid::unit(offset_x_pt, "pt")
  y_right <- grid::unit(y, "npc") - grid::unit(offset_y_pt, "pt")

  x_left  <- grid::unit(x, "npc") + grid::unit(offset_x_pt, "pt")
  y_left  <- grid::unit(y, "npc") + grid::unit(offset_y_pt, "pt")

  right_line <- grid::polylineGrob(
    x = x_right,
    y = y_right,
    gp = grid::gpar(
      col = col1,
      lwd = lwd / 2,
      lineend = lineend,
      linejoin = linejoin
    )
  )

  left_line <- grid::polylineGrob(
    x = x_left,
    y = y_left,
    gp = grid::gpar(
      col = col2,
      lwd = lwd / 2,
      lineend = lineend,
      linejoin = linejoin
    )
  )

  grid::gTree(children = grid::gList(right_line, left_line))
}
