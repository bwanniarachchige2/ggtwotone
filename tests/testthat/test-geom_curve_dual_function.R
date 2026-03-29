test_that("geom_dual_function works with basic function", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      colour1 = "white",
      colour2 = "black"
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function computes fallback contrast color", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      colour1 = "#FFFFFF",
      colour2 = NULL
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function works with smooth = TRUE", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-3, 3),
      smooth = TRUE,
      linewidth = 1,
      colour1 = "#FFFFFF",
      colour2 = "#000000"
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function handles invalid function args", {
  warnings <- character()

  withCallingHandlers(
    {
      ggplot2::ggplot() +
        geom_dual_function(
          fun = dt,
          xlim = c(-2, 2),
          args = list(unknown = 5)
        )
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(
    any(grepl("evaluate function", warnings)),
    info = paste("Captured warnings were:", paste(warnings, collapse = "; "))
  )
})

test_that("geom_dual_function works with base_color branch", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      base_color = "green",
      smooth = TRUE
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function accepts color1 and color2 aliases", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      color1 = "yellow",
      color2 = "blue",
      smooth = TRUE
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function uses default colors when none supplied", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      smooth = TRUE
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function works with smooth = FALSE", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      smooth = FALSE,
      colour1 = "white",
      colour2 = "black"
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function enforces ncp minimum in non-smooth mode", {
  p <- ggplot2::ggplot() +
    geom_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      smooth = FALSE,
      ncp = 1,
      colour1 = "white",
      colour2 = "black"
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_dual_function returns blank geom when function gives too few finite values", {
  lyr <- geom_dual_function(
    fun = function(x) rep(NA_real_, length(x)),
    xlim = c(-2, 2),
    smooth = TRUE
  )

  expect_s3_class(lyr$geom, "GeomBlank")
})

test_that("GeomPathDualSide draw_panel returns nullGrob for fewer than 2 points", {
  df <- data.frame(
    x = 0.5,
    y = 0.5,
    group = 1,
    colour1 = "white",
    colour2 = "black",
    linewidth = 1.2,
    alpha = 1,
    lineend = "round",
    linejoin = "round",
    linemitre = 10
  )

  panel_params <- ggplot2::ggplot_build(
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_blank()
  )$layout$panel_params[[1]]

  g <- GeomPathDualSide$draw_panel(
    data = df,
    panel_params = panel_params,
    coord = ggplot2::coord_cartesian()
  )

  expect_s3_class(g, "null")
})

test_that("GeomPathDualSide draw_panel returns grob for valid path", {
  df <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 1, 0),
    group = c(1, 1, 1),
    colour1 = "white",
    colour2 = "black",
    linewidth = 1.2,
    alpha = 1,
    lineend = "round",
    linejoin = "round",
    linemitre = 10
  )

  panel_params <- ggplot2::ggplot_build(
    ggplot2::ggplot(data.frame(x = c(0, 2), y = c(0, 1)), ggplot2::aes(x, y)) +
      ggplot2::geom_blank()
  )$layout$panel_params[[1]]

  g <- GeomPathDualSide$draw_panel(
    data = df,
    panel_params = panel_params,
    coord = ggplot2::coord_cartesian()
  )

  expect_s3_class(g, "gTree")
})

test_that("GeomPathDualSide draw_panel handles grouped paths", {
  df <- data.frame(
    x = c(0, 1, 2, 0, 1, 2),
    y = c(0, 1, 0, 1, 0, 1),
    group = c(1, 1, 1, 2, 2, 2),
    colour1 = "white",
    colour2 = "black",
    linewidth = 1.2,
    alpha = 1,
    lineend = "round",
    linejoin = "round",
    linemitre = 10
  )

  panel_params <- ggplot2::ggplot_build(
    ggplot2::ggplot(data.frame(x = c(0, 2), y = c(0, 1)), ggplot2::aes(x, y)) +
      ggplot2::geom_blank()
  )$layout$panel_params[[1]]

  g <- GeomPathDualSide$draw_panel(
    data = df,
    panel_params = panel_params,
    coord = ggplot2::coord_cartesian()
  )

  expect_s3_class(g, "gTree")
})
