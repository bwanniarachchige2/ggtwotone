#' Adjust Contrast Between Two Stroke Colors
#'
#' Given a base color and a background, generate a pair of colors (light and dark)
#' with sufficient perceptual contrast using WCAG or APCA methods.
#'
#' @param color A base color, as a hex string or valid R color name (e.g., "#6699CC", "darkred").
#' @param contrast Minimum desired contrast ratio (default is 4.5).
#' @param method Contrast method: "WCAG", "APCA", or "auto" to try both.
#' @param background Background color, as a hex string or valid R color name (default: "#FFFFFF").
#' @param quiet Logical. If TRUE, suppresses warnings.
#'
#' @return A list with elements `light`, `dark`, `contrast`, and `method`.
#'
#' @examples
#' adjust_contrast_pair("#777777", contrast = 4.5, method = "auto", background = "#000000")
#'
#' adjust_contrast_pair("#66CCFF", contrast = 4.5, method = "APCA", background = "#FAFAFA")
#' @export
adjust_contrast_pair <- function(color, contrast = 4.5, method = "auto",
                                 background = "#FFFFFF", quiet = FALSE) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required for contrast adjustment.")
  }

  if (!is.character(color) || length(color) != 1 || is.na(color)) {
    warning("Invalid color input.")
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  if (!is.character(background) || length(background) != 1 || is.na(background)) {
    warning("Invalid background input.")
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  # Keep originals for clear warnings
  orig_color <- color
  orig_background <- background

  # Normalize both to hex (handles names like "darkred")
  color <- safe_as_hex(color)[1]
  background <- safe_as_hex(background)[1]

  # Short-circuit invalid background
  if (is.na(background)) {
    if (!quiet) warning(sprintf("Invalid background color: %s", orig_background))
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  # Short-circuit invalid base color
  if (is.na(color)) {
    if (!quiet) warning(sprintf("Could not convert base color %s to HCL.", orig_color))
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  # Try converting base color to HCL
  base_hcl <- if (quiet) {
    tryCatch({
      suppressWarnings(as(colorspace::hex2RGB(color), "polarLUV")@coords)
    }, error = function(e) NULL)
  } else {
    tryCatch({
      as(colorspace::hex2RGB(color), "polarLUV")@coords
    }, error = function(e) {
      warning(sprintf("Could not convert base color %s to HCL.", color))
      NULL
    })
  }

  if (is.null(base_hcl) || !is.numeric(base_hcl) || length(base_hcl) < 2) {
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  # Validate background early using col2rgb
  valid_bg <- tryCatch({
    grDevices::col2rgb(background)
    TRUE
  }, error = function(e) {
    if (!quiet) warning(sprintf("Invalid background color: %s", background))
    FALSE
  })

  if (!valid_bg) {
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  hcl_names <- colnames(base_hcl)
  h <- base_hcl[hcl_names=="H"]
  c <- base_hcl[hcl_names=="C"]
  try_l <- seq(20, 95, by = 5)
  candidates <- colorspace::hex(colorspace::polarLUV(L = try_l, C = c, H = h), fixup =TRUE)

  get_contrast <- function(fg, bg, method) {
    result <- tryCatch({
      cr <- colorspace::contrast_ratio(fg, bg, algorithm = method)
      if (method == "APCA") {
        if (is.matrix(cr)) abs(cr[, "normal"]) else abs(cr["normal"])
      } else {
        cr
      }
    }, error = function(e) {
      warning(sprintf("computation failed for %s vs %s [%s]: %s", fg, bg, method, e$message))
      NA
    })
    return(result)
  }

  contrast_methods <- if (method == "auto") c("WCAG", "APCA") else method
  best_result <- NULL

  for (m in contrast_methods) {
    contrasts <- sapply(candidates, get_contrast, bg = background, method = m)
    best_light <- candidates[which.min(contrasts)]
    best_dark <- candidates[which.max(contrasts)]
    max_contrast <- max(contrasts, na.rm = TRUE)

    if (!is.na(max_contrast) && max_contrast >= contrast) {
      best_result <- list(light = best_light, dark = best_dark, contrast = max_contrast, method = m)
      break
    }
  }

  if (is.null(best_result)) {
    warning(sprintf("No sufficient contrast found for base color %s. Using fallback colors.", color))
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  return(best_result)
}
