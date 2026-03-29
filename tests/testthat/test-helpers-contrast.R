test_that("safe_as_hex converts valid colors", {
  out <- safe_as_hex(c("red", "blue"))

  expect_length(out, 2)
  expect_true(all(!is.na(out)))
  expect_match(out[1], "^#")
  expect_match(out[2], "^#")
})

test_that("safe_as_hex returns all NA when any input color is invalid", {
  out <- safe_as_hex(c("red", "notacolor"))

  expect_length(out, 2)
  expect_true(all(is.na(out)))
})

test_that("get_contrast returns numeric WCAG contrast for valid colors", {
  out <- get_contrast("#000000", "#FFFFFF", method = "WCAG")

  expect_true(is.numeric(out))
  expect_false(is.na(out))
  expect_gte(out, 1)
})

test_that("get_contrast returns numeric APCA contrast for valid colors", {
  out <- get_contrast("#000000", "#FFFFFF", method = "APCA")

  expect_true(is.numeric(out))
  expect_false(is.na(out))
})

test_that("get_contrast returns NA for invalid colors", {
  out <- get_contrast("notacolor", "#FFFFFF", method = "WCAG")

  expect_true(is.na(out))
})

test_that("compute_best_text_color returns one of the candidate colors", {
  out <- compute_best_text_color(
    bg_color = c("#000000", "#FFFFFF", "#777777"),
    method = "WCAG",
    light = "#eeeeee",
    dark = "#111111"
  )

  expect_length(out, 3)
  expect_true(all(out %in% c("#eeeeee", "#111111")))
})

test_that("compute_best_text_color handles invalid background colors", {
  out <- compute_best_text_color(
    bg_color = c("notacolor", "#FFFFFF"),
    method = "WCAG",
    light = "#eeeeee",
    dark = "#111111"
  )

  expect_length(out, 2)
  expect_true(all(out %in% c("#eeeeee", "#111111")))
})

test_that("two_colour_path_grob returns empty gTree for fewer than 2 points", {
  g <- two_colour_path_grob(
    x = 0.5,
    y = 0.5,
    col1 = "black",
    col2 = "white",
    lwd = 2
  )

  expect_s3_class(g, "gTree")
  expect_equal(length(g$children), 0)
})

test_that("two_colour_path_grob returns empty gTree for zero-length path", {
  g <- two_colour_path_grob(
    x = c(0.5, 0.5, 0.5),
    y = c(0.5, 0.5, 0.5),
    col1 = "black",
    col2 = "white",
    lwd = 2
  )

  expect_s3_class(g, "gTree")
  expect_equal(length(g$children), 0)
})

test_that("two_colour_path_grob returns grob with
          two children for valid path", {
  g <- two_colour_path_grob(
    x = c(0.1, 0.5, 0.9),
    y = c(0.2, 0.8, 0.3),
    col1 = "black",
    col2 = "white",
    lwd = 2
  )

  expect_s3_class(g, "gTree")
  expect_equal(length(g$children), 2)
})

test_that("two_colour_path_grob works for two-point path", {
  g <- two_colour_path_grob(
    x = c(0.1, 0.9),
    y = c(0.2, 0.8),
    col1 = "red",
    col2 = "blue",
    lwd = 3
  )

  expect_s3_class(g, "gTree")
  expect_equal(length(g$children), 2)
})

test_that("two_colour_path_grob handles paths with one zero-length segment", {
  g <- two_colour_path_grob(
    x = c(0.1, 0.1, 0.8),
    y = c(0.2, 0.2, 0.7),
    col1 = "red",
    col2 = "blue",
    lwd = 2
  )

  expect_s3_class(g, "gTree")
})
