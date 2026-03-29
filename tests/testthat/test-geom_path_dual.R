test_that("geom_path_dual basic rendering", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()

  p1 <- ggplot(mtcars,
            aes(x = wt, y = mpg, xend = wt + 0.5, yend = mpg + 2)) +
    geom_path_dual(
      colour1 = "black",
      colour2 = "white",
      linewidth = 1.5
    )

  vdiffr::expect_doppelganger(
    "geom-path-dual-basic",
    p1
  )
})

test_that("geom_path_dual handles NA color1 without warning", {
  df <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(1, 3, 2, 4)
  )
  p <- ggplot2::ggplot(df, aes(x, y)) +
    geom_path_dual(
      colour1 = NA,
      colour2 = "#000000",
      linewidth = 1
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_path_dual errors with missing aesthetics", {
  df <- data.frame(x = 1:4)

  p <- ggplot2::ggplot(df) +
    geom_path_dual(aes(x = x))  # missing y

  expect_error(
    ggplot2::ggplot_build(p),
    regexp = "y"
  )
})

test_that("geom_path_dual does not error on invalid colors", {
  p <- ggplot2::ggplot(
    data.frame(x = c(1, 2, 3, 4),
               y = c(1, 3, 2, 4)),
    aes(x, y)
  ) +
    geom_path_dual(
      colour1 = "notacolor",
      colour2 = "gray",
      linewidth = 2
    )

  expect_no_error(ggplot2::ggplot_build(p))
})
