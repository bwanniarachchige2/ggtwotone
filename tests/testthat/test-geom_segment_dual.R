test_that("geom_segment_dual throws error with missing aesthetics", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3)) +
    geom_segment_dual(
      aes(x = x, y = y),
      colour1 = "black",
      colour2 = "gray",
      linewidth = 2)  # missing xend/yend
  expect_error(ggplot2::ggplot_build(p), regexp = "xend")
})

test_that("geom_segment_dual works with minimal valid input", {
  p <- ggplot2::ggplot() +
    geom_segment_dual(
      aes(x = 1, y = 1, xend = 2, yend = 2),
      colour1 = "#FF0000",
      colour2 = "#000000",
      linewidth = 1
    )
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual handles NA color1 without warning", {
  p <- ggplot2::ggplot() +
    geom_segment_dual(
      aes(x = 1, y = 1, xend = 2, yend = 2),
      colour1 = NA,
      colour2 = "#000000",
      linewidth = 1
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_segment_dual basic rendering", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()

  p <- ggplot(mtcars) +
    geom_segment_dual(
      aes(x = wt, y = mpg, xend = wt + 0.5, yend = mpg + 2),
      colour1 = "black",
      colour2 = "white",
      linewidth = 1.5
    )

  vdiffr::expect_doppelganger(
    "geom-segment-dual-basic",
    p
  )
})

