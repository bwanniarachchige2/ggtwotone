test_that("geom_curve_dual works with minimal input", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual applies custom stroke colors", {
  df <- data.frame(x = 1, y = 1, xend = 3, yend = 3)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      color1 = "yellow", color2 = "blue"
    )

  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built$plot$layers[[1]]$geom, "GeomCurveDual")
})

test_that("geom_curve_dual skips rows with NA gracefully", {
  df <- data.frame(
    x = c(1, NA),
    y = c(1, 2),
    xend = c(3, 4),
    yend = c(3, NA)
  )

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual throws error when xend or yend are missing", {
  df <- data.frame(x = 1, y = 1)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(ggplot2::aes(x = x, y = y))

  expect_error(ggplot2::ggplot_build(p), regexp = "xend|yend|missing")
})

test_that("geom_curve_dual works with base_color branch", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      base_color = "red"
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual injects colour mapping when base_color is given", {
  lyr <- geom_curve_dual(
    ggplot2::aes(x = 1, y = 1, xend = 2, yend = 2),
    base_color = "red"
  )

  expect_true("colour1" %in% names(lyr$mapping))
  expect_true("colour2" %in% names(lyr$mapping))
})

test_that("geom_curve_dual injects curvature angle and ncp into mapping", {
  lyr <- geom_curve_dual(
    ggplot2::aes(x = 1, y = 1, xend = 2, yend = 2),
    curvature = 0.5,
    angle = 60,
    ncp = 7
  )

  expect_true("curvature" %in% names(lyr$mapping))
  expect_true("angle" %in% names(lyr$mapping))
  expect_true("ncp" %in% names(lyr$mapping))
})

test_that("geom_curve_dual handles ncp less than 3", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      ncp = 2
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual handles alpha aesthetic", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2, alpha = 0.4)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, alpha = alpha)
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual handles custom curvature and angle", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      curvature = -0.4,
      angle = 45
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual handles all-incomplete rows gracefully", {
  df <- data.frame(
    x = c(NA_real_, NA_real_),
    y = c(1, 2),
    xend = c(2, 3),
    yend = c(NA_real_, NA_real_)
  )

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour1 = "white",
      colour2 = "black"
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual works with explicit parameters on valid row", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour1 = "white",
      colour2 = "black",
      linewidth = 1.2,
      curvature = 0.3,
      angle = 90,
      ncp = 21,
      alpha = 1,
      lineend = "round"
    )

  expect_silent(ggplot2::ggplot_build(p))
})
