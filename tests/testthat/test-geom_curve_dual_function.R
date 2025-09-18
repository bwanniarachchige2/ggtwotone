test_that("geom_curve_dual_function works with basic function", {
  p <- ggplot2::ggplot() +
    geom_curve_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      colour1 = "white",
      colour2 = "black"
    )
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual_function computes fallback contrast color", {
  p <- ggplot2::ggplot() +
    geom_curve_dual_function(
      fun = dnorm,
      xlim = c(-2, 2),
      colour1 = "#FFFFFF",
      colour2 = NULL
      #background = "#000000"
    )
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual_function works with smooth = TRUE", {
  p <- ggplot2::ggplot() +
    geom_curve_dual_function(
      fun = dnorm,
      xlim = c(-3, 3),
      smooth = TRUE,
      linewidth = 1,
      colour1 = "#FFFFFF",
      colour2 = "#000000"
      #offset = 0.003
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual_function handles invalid function args", {
  warnings <- character()

  withCallingHandlers(
    {
      ggplot2::ggplot() +
        geom_curve_dual_function(
          fun = dt,
          xlim = c(-2, 2),
          args = list(unknown = 5)  # invalid arg
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


