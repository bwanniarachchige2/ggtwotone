test_that("highlight_colors returns requested number of colors", {
  set.seed(1)
  pal <- highlight_colors(
    n = 3,
    background = "#222222",
    base_color = "#EEEEEE",
    quiet = TRUE
  )

  expect_length(pal, 3)
  expect_true(all(grepl("^#[0-9A-F]{6,8}$", pal)))
})

test_that("highlight_info returns metadata", {
  set.seed(1)
  pal <- highlight_colors(
    n = 3,
    background = "#222222",
    base_color = "#EEEEEE",
    quiet = TRUE
  )

  info <- highlight_info(pal)

  expect_type(info, "list")
  expect_true("n_selected" %in% names(info))
  expect_equal(info$n_selected, length(pal))
})

test_that("highlight_colors errors on invalid colors", {
  expect_error(
    highlight_colors(
      n = 3,
      background = "notacolor",
      base_color = "#FFFFFF",
      quiet = TRUE
    ),
    "Invalid"
  )
})

test_that("highlight_colors works with contrast_base = 0", {
  set.seed(1)
  pal <- highlight_colors(
    n = 3,
    background = "#222222",
    base_color = "#EEEEEE",
    contrast_base = 0,
    quiet = TRUE
  )

  expect_length(pal, 3)
})

test_that("highlight_colors errors on invalid hue_targets", {
  expect_error(
    highlight_colors(
      n = 3,
      background = "#222222",
      base_color = "#EEEEEE",
      hue_targets = list(1, 2, 3),
      quiet = TRUE
    ),
    "`hue_targets`"
  )
})

test_that("highlight_colors errors when too few candidates are available", {
  expect_error(
    highlight_colors(
      n = 50,
      background = "#222222",
      base_color = "#EEEEEE",
      n_candidates = 10,
      relax = FALSE,
      quiet = TRUE
    ),
    "Too few in-gamut candidates"
  )
})

test_that("highlight_colors records auto contrast method", {
  set.seed(1)
  pal <- highlight_colors(
    n = 3,
    background = "#F7F7F7",
    base_color = "#222222",
    contrast_method = "auto",
    quiet = TRUE
  )

  info <- highlight_info(pal)
  expect_true(info$contrast_method %in% c("WCAG", "APCA"))
})

test_that("highlight_colors errors when n is invalid", {
  expect_error(
    highlight_colors(
      n = 0,
      background = "#222222",
      base_color = "#EEEEEE",
      quiet = TRUE
    )
  )
})
