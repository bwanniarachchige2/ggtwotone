test_that("geom_segment_dual throws error with missing aesthetics", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3)) +
    geom_segment_dual(
      ggplot2::aes(x = x, y = y),
      colour1 = "black",
      colour2 = "gray",
      linewidth = 2
    )

  expect_error(ggplot2::ggplot_build(p), regexp = "xend")
})

test_that("geom_segment_dual works with minimal valid input", {
  p <- ggplot2::ggplot() +
    geom_segment_dual(
      ggplot2::aes(x = 1, y = 1, xend = 2, yend = 2),
      colour1 = "#FF0000",
      colour2 = "#000000",
      linewidth = 1
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual handles NA colour1 without warning", {
  p <- ggplot2::ggplot() +
    geom_segment_dual(
      ggplot2::aes(x = 1, y = 1, xend = 2, yend = 2),
      colour1 = NA,
      colour2 = "#000000",
      linewidth = 1
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual works with default colours when none supplied", {
  p <- ggplot2::ggplot(
    data.frame(x = 1, y = 1, xend = 2, yend = 2),
    ggplot2::aes(x, y, xend = xend, yend = yend)
  ) +
    geom_segment_dual(linewidth = 1)

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual works with mapped colour1 and colour2 columns", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    xend = c(2, 3),
    yend = c(2, 1),
    colour1 = c("black", "red"),
    colour2 = c("white", "blue")
  )

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = x, y = y, xend = xend, yend = yend,
      colour1 = colour1, colour2 = colour2
    )
  ) +
    geom_segment_dual(linewidth = 1)

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual handles alpha values", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2, alpha = 0.4)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend, alpha = alpha)
  ) +
    geom_segment_dual(
      colour1 = "black",
      colour2 = "white",
      linewidth = 1
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual handles multiple segments", {
  df <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 2, 1),
    xend = c(2, 3, 4),
    yend = c(2, 1, 2)
  )

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ) +
    geom_segment_dual(
      colour1 = "black",
      colour2 = "white",
      linewidth = 1
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual handles zero-length segments", {
  df <- data.frame(x = 1, y = 1, xend = 1, yend = 1)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ) +
    geom_segment_dual(
      colour1 = "black",
      colour2 = "white",
      linewidth = 1
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual handles arrows and arrow.fill", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ) +
    geom_segment_dual(
      colour1 = "black",
      colour2 = "white",
      linewidth = 1,
      arrow = grid::arrow(length = grid::unit(0.1, "inches"), type = "closed"),
      arrow.fill = "orange"
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("two_colour_segment_grob returns empty gTree for
          zero-length segment", {
  g <- two_colour_segment_grob(
    x0 = 0.5, y0 = 0.5, x1 = 0.5, y1 = 0.5,
    col1 = "black", col2 = "white",
    lwd = 2, lineend = "butt",
    arrow = NULL, arrow.fill = NULL
  )

  expect_s3_class(g, "gTree")
  expect_equal(length(g$children), 0)
})

test_that("two_colour_segment_grob returns two children for valid segment", {
  g <- two_colour_segment_grob(
    x0 = 0.1, y0 = 0.2, x1 = 0.9, y1 = 0.8,
    col1 = "black", col2 = "white",
    lwd = 2, lineend = "round",
    arrow = NULL, arrow.fill = NULL
  )

  expect_s3_class(g, "gTree")
  expect_equal(length(g$children), 2)
})

test_that("two_colour_segment_grob handles arrow.fill branch", {
  g <- two_colour_segment_grob(
    x0 = 0.1, y0 = 0.2, x1 = 0.9, y1 = 0.8,
    col1 = "black", col2 = "white",
    lwd = 2, lineend = "round",
    arrow = grid::arrow(length = grid::unit(0.1, "inches"), type = "closed"),
    arrow.fill = "orange"
  )

  expect_s3_class(g, "gTree")
  expect_equal(length(g$children), 2)
})

test_that("geom_segment_dual basic rendering", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()

  p <- ggplot2::ggplot(mtcars) +
    geom_segment_dual(
      ggplot2::aes(x = wt, y = mpg, xend = wt + 0.5, yend = mpg + 2),
      colour1 = "black",
      colour2 = "white",
      linewidth = 1.5
    )

  vdiffr::expect_doppelganger("geom-segment-dual-basic", p)
})
