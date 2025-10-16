# -----------------------------------------------------------------------------
# Contrast backends (WCAG or APCA), selected via:
#   options(ggtwotone.contrast_method = "WCAG")  # default
#   options(ggtwotone.contrast_method = "APCA")
# Or let highlight_colors(auto) choose per polarity.
# -----------------------------------------------------------------------------

# sRGB -> linearized RGB -> relative luminance Y (0..1)
.y_from_hex <- function(hex) {
  rgb01 <- t(grDevices::col2rgb(hex)) / 255
  lin   <- ifelse(rgb01 <= 0.03928, rgb01/12.92, ((rgb01 + 0.055)/1.055)^2.4)
  drop(0.2126*lin[,1] + 0.7152*lin[,2] + 0.0722*lin[,3])
}

# WCAG (ratio)
.wcag_ratio <- function(col, ref) {
  y1 <- .y_from_hex(col); y2 <- .y_from_hex(ref)
  (max(y1, y2) + 0.05) / (min(y1, y2) + 0.05)
}

# APCA absolute Lc (0..~106). Uses coloratio::apca if available; otherwise a compact approximation.
.apca_Lc_abs <- function(col, ref) {
  if (requireNamespace("coloratio", quietly = TRUE)) {
    return(abs(coloratio::apca(col, ref)))
  }
  y_txt <- .y_from_hex(col)
  y_bg  <- .y_from_hex(ref)
  if (y_bg >= y_txt) {
    Lc <- ( (y_bg^0.56 - y_txt^0.57) * 1.14 ) * 100
  } else {
    Lc <- ( (y_txt^0.62 - y_bg^0.65) * 1.14 ) * 100
  }
  abs(Lc)
}

# Unified contrast entry that highlight_colors() will call internally
.contrast_value <- function(col, ref) {
  method <- getOption("ggtwotone.contrast_method", "WCAG")
  if (identical(toupper(method), "APCA")) .apca_Lc_abs(col, ref) else .wcag_ratio(col, ref)
}

#' @keywords internal
safe_as_hex <- function(col) {
  if (length(col) == 0) return(character(0))
  tryCatch({
    mat <- grDevices::col2rgb(col, alpha = TRUE)  # 4 x n (RGBA 0..255)
    r <- mat[1, ] / 255; g <- mat[2, ] / 255; b <- mat[3, ] / 255
    a <- mat[4, ] / 255
    grDevices::rgb(r, g, b, a)                    # -> #RRGGBBAA (alpha==1 gives #RRGGBB)
  }, error = function(e) rep(NA_character_, length(col)))
}

#' Generate a high-contrast, well-separated highlight palette (global search)
#'
#' Changes from your previous version:
#' - Global hue sampling by default (`hue_targets = "auto"`)
#' - Optional hue repulsion near background/base (`repel_from`, `repel_band`)
#' - Smart auto-bias toward opponent hues when bg & base are far apart (`auto_bias`)
#' - NEW: `contrast_method="auto"` picks APCA for dark-on-light and WCAG for light-on-dark
#'
#' Creates n colors by:
#'  (1) sampling HCL colors (global or biased),
#'  (2) filtering by WCAG/APCA contrast vs `background` (and optionally vs `base_color`),
#'  (3) selecting a maximally-separated subset by ΔE2000 and minimum hue spacing.
#'
#' Set `contrast_base = 0` to ignore base contrast (keeps only background contrast).
#' Use `hue_targets = "anchored"` to sample around `base_color` (legacy-like).
#'
#' NOTE: Switch contrast backend via:
#'   options(ggtwotone.contrast_method = "WCAG")  # ratio scale, e.g. 4.5
#'   options(ggtwotone.contrast_method = "APCA")  # Lc scale, e.g. 60
#'   OR pass contrast_method="auto" to pick per polarity.
#'
#' @param n Integer, number of colors.
#' @param background Hex/R color for the plot background.
#' @param base_color Hex/R color used for non-highlight elements.
#' @param contrast_bg Required contrast vs background (WCAG ratio or APCA Lc depending on backend). Default 4.5.
#' @param contrast_base Required contrast vs base_color (set 0 to skip). Default 3.0.
#' @param min_deltaE Minimum CIEDE2000 separation between selected colors. Default 35.
#' @param hue_targets "auto", "anchored", "spread", or numeric vector of hue angles (0–360).
#' @param anchor_band Degrees around base hue to sample when hue_targets="anchored". Default 180.
#' @param hcl_L_range Allowed HCL lightness range. Default c(25, 75).
#' @param hcl_C_range Allowed HCL chroma range. Default c(40, 90).
#' @param min_hue_sep Minimum separation in degrees between chosen hues. Default 35.
#' @param n_candidates Number of random candidates before selection. Default 100000.
#' @param relax Progressively relax min_hue_sep then min_deltaE if infeasible. Default TRUE.
#' @param quiet Suppress relaxation messages. Default TRUE.
#' @param repel_from Character vector among c("background","base") to avoid those hue neighborhoods.
#' @param repel_band Degrees excluded around the repelled hues (±band). Default 50.
#' @param auto_bias Logical; if TRUE and bg/base are far apart, bias sampling toward opponent hues.
#' @param bias_width Width (degrees) of opponent sampling window when auto_bias applies. Default 120.
#' @param bias_frac Fraction of samples drawn from the biased window when auto_bias applies. Default 0.8.
#' @param contrast_method "option" (use global option, default) or "auto" (pick WCAG/APCA per polarity).
#' @return Character vector of hex colors (length <= n) with an `info` attribute.
#' @examples
#' highlight_colors(
#'  n = 4,
#'  background = "#222222",
#'  base_color = "#eeeeee")
#'
#' # ---------------------------------------------------------------
#' # dark base on light background
#' # ---------------------------------------------------------------
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'
#'   bg_hex   <- "#F7F7F7"   # light background
#'   base_hex <- "#222222"   # dark base
#'
#'   set.seed(7)
#'   pal <- highlight_colors(
#'     n = 3,
#'     background     = bg_hex,
#'     base_color     = base_hex,
#'     contrast_method= "auto",   # automatically selects APCA
#'     contrast_bg    = 60,
#'     contrast_base  = 45,
#'     quiet = TRUE
#'   )
#'
#'   print(pal)
#'
#'   classes <- c("compact","suv","midsize","pickup","minivan","2seater")
#'   counts  <- c(25, 38, 42, 31, 10, 5)
#'   highlight_classes <- c("compact","suv","midsize")
#'
#'   col_map <- setNames(rep(base_hex, length(classes)), classes)
#'   col_map[highlight_classes] <- pal
#'
#'   df <- data.frame(class = classes, count = counts, fill = unname(col_map[classes]))
#'
#'   ggplot(df, aes(x = reorder(class, count), y = count, fill = fill)) +
#'     geom_col(width = 0.8) +
#'     scale_fill_identity() +
#'     coord_flip() +
#'     labs(title = "Dark base on light background (auto → APCA)",
#'          x = NULL, y = "Count") +
#'     theme_minimal(base_size = 12) +
#'     theme(
#'       panel.grid.major.y = element_blank(),
#'       plot.background  = element_rect(fill = bg_hex, color = NA),
#'       panel.background = element_rect(fill = bg_hex, color = NA),
#'       plot.title = element_text(color = base_hex, hjust = 0.5),
#'       axis.text  = element_text(color = base_hex)
#'     )
#' }
#'
#' # ---------------------------------------------------------------
#' # light base on dark background
#' # ---------------------------------------------------------------
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'
#'   bg_hex   <- "#222222"   # dark background
#'   base_hex <- "#EEEEEE"   # light base
#'
#'   set.seed(7)
#'   pal <- highlight_colors(
#'     n = 3,
#'     background     = bg_hex,
#'     base_color     = base_hex,
#'     contrast_method= "auto",   # automatically selects WCAG
#'     contrast_bg    = 4.5,
#'     contrast_base  = 3.0,
#'     quiet = TRUE
#'   )
#'
#'   print(pal)
#'
#'   classes <- c("compact","suv","midsize","pickup","minivan","2seater")
#'   counts  <- c(25, 38, 42, 31, 10, 5)
#'   highlight_classes <- c("compact","suv","midsize")
#'
#'   col_map <- setNames(rep(base_hex, length(classes)), classes)
#'   col_map[highlight_classes] <- pal
#'
#'   df <- data.frame(class = classes, count = counts, fill = unname(col_map[classes]))
#'
#'   ggplot(df, aes(x = reorder(class, count), y = count, fill = fill)) +
#'     geom_col(width = 0.8) +
#'     scale_fill_identity() +
#'     coord_flip() +
#'     labs(title = "Light base on dark background (auto → WCAG)",
#'          x = NULL, y = "Count") +
#'     theme_minimal(base_size = 12) +
#'     theme(
#'       panel.grid.major.y = element_blank(),
#'       plot.background  = element_rect(fill = bg_hex, color = NA),
#'       panel.background = element_rect(fill = bg_hex, color = NA),
#'       plot.title = element_text(color = base_hex, hjust = 0.5),
#'       axis.text  = element_text(color = base_hex)
#'     )
#' }
#' @export
highlight_colors <- function(
    n,
    background    = "#F0F0F0",
    base_color    = "#4a1919",
    contrast_bg   = 4.5,
    contrast_base = 3.0,
    min_deltaE    = 35,
    hue_targets   = "auto",
    anchor_band   = 180,
    hcl_L_range   = c(25, 75),
    hcl_C_range   = c(40, 90),
    min_hue_sep   = 35,
    n_candidates  = 100000,
    relax         = TRUE,
    quiet         = TRUE,
    repel_from    = c("background","base"),
    repel_band    = 50,
    auto_bias     = TRUE,
    bias_width    = 120,
    bias_frac     = 0.8,
    contrast_method = c("option","auto")
) {
  contrast_method <- match.arg(contrast_method)

  stopifnot(is.numeric(n), length(n) == 1, n >= 1)

  # Normalize user colors
  bg_hex   <- safe_as_hex(background)[1]
  base_hex <- safe_as_hex(base_color)[1]
  if (is.na(bg_hex) || is.na(base_hex)) stop("Invalid `background` or `base_color`.")
  background <- bg_hex
  base_color <- base_hex

  # --- auto-contrast engine (local, restores on exit) -----------------------
  selected_backend <- getOption("ggtwotone.contrast_method", "WCAG")
  if (identical(contrast_method, "auto")) {
    y_bg   <- .y_from_hex(background)
    y_base <- .y_from_hex(base_color)
    # If base is darker than background -> dark-on-light -> APCA; else WCAG
    selected_backend <- if (is.finite(y_bg) && is.finite(y_base) && y_base < y_bg) "APCA" else "WCAG"
    old_backend <- getOption("ggtwotone.contrast_method", "WCAG")
    options(ggtwotone.contrast_method = selected_backend)
    on.exit(options(ggtwotone.contrast_method = old_backend), add = TRUE)
  }

  # --- guardrail: if WCAG & requested contrast vs bg is only possible on the light side ----
  # Only meaningful for WCAG ratio (symmetric). Skip for APCA.
  if (identical(toupper(selected_backend), "WCAG")) {
    bg_linL <- (function(hex) {
      rgb01 <- t(grDevices::col2rgb(hex))/255
      lin   <- ifelse(rgb01 <= 0.03928, rgb01/12.92, ((rgb01+0.055)/1.055)^2.4)
      drop(0.2126*lin[,1] + 0.7152*lin[,2] + 0.0722*lin[,3])
    })(background)
    max_if_lighter <- 1.05 / (bg_linL + 0.05)      # max WCAG ratio by being lighter than bg
    if (contrast_bg > max_if_lighter) {
      hcl_L_range <- c(min(5, hcl_L_range[1]), max(20, min(40, hcl_L_range[2])))
      hcl_C_range <- c(0, max(40, hcl_C_range[2]))
      if ("background" %in% repel_from) repel_from <- setdiff(repel_from, "background")
      repel_band <- min(repel_band, 30)
      min_deltaE <- min(min_deltaE, 28)
      min_hue_sep <- min(min_hue_sep, 28)
      if (!quiet) message("Strict contrast vs this background forces near-black highlights; ",
                          "expanding search to dark, low-chroma region.")
    }
  }

  # effective thresholds (for info)
  contrast_bg_eff   <- contrast_bg
  contrast_base_eff <- contrast_base
  cur_min_deltaE    <- min_deltaE
  cur_min_hsep      <- min_hue_sep

  # helpers
  wcag_contrast_one <- function(col, ref) .contrast_value(col, ref)  # <-- unified backend
  circ_dist <- function(a, b) { d <- abs(a - b) %% 360; pmin(d, 360 - d) }
  hue_dist_ok <- function(h1, h2, thr) circ_dist(h1, h2) >= thr

  # get hues
  base_h <- tryCatch(farver::decode_colour(base_color, to = "hcl")[, 1], error = function(e) NA_real_)
  bg_h   <- tryCatch(farver::decode_colour(background, to = "hcl")[, 1],    error = function(e) NA_real_)

  # --- hue sampling ----------------------------------------------------------
  if (is.character(hue_targets)) {
    hue_mode <- match.arg(hue_targets, c("auto","anchored","spread"))

    if (hue_mode %in% c("auto","spread")) {
      # global sampling, with optional auto-bias
      if (isTRUE(auto_bias) && is.finite(bg_h) && is.finite(base_h) && circ_dist(bg_h, base_h) > 100) {
        centers <- c((bg_h + 180) %% 360, (base_h + 180) %% 360)
        half_w <- pmin(180, pmax(10, bias_width) / 2)
        n_biased <- floor(n_candidates * bias_frac)
        n_each   <- max(1, floor(n_biased / 2))
        n_global <- max(0, n_candidates - 2 * n_each)
        H <- c(
          (centers[1] + stats::runif(n_each, -half_w, half_w)) %% 360,
          (centers[2] + stats::runif(n_each, -half_w, half_w)) %% 360,
          stats::runif(n_global, 0, 360)
        )
        mode_used <- "global+dual-opponent"
      } else {
        H <- stats::runif(n_candidates, 0, 360)
        mode_used <- "global"
      }
    } else { # anchored
      width  <- max(30, min(360, anchor_band * 2))
      center <- if (is.na(base_h)) 0 else base_h
      H <- (center + stats::runif(n_candidates, -width/2, width/2)) %% 360
      mode_used <- "anchored"
    }
  } else if (is.numeric(hue_targets)) {
    H <- as.numeric(sample(hue_targets %% 360, size = n_candidates, replace = TRUE))
    mode_used <- "numeric"
  } else {
    stop('`hue_targets` must be "auto", "anchored", "spread", or numeric angles.')
  }

  # sample L, C
  L <- stats::runif(length(H), min(hcl_L_range), max(hcl_L_range))
  C <- stats::runif(length(H), min(hcl_C_range), max(hcl_C_range))

  # to hex, clip to gamut
  cand_hex_full <- colorspace::hex(colorspace::polarLUV(L, C, H), fix = TRUE)
  cand_hex_full <- unique(cand_hex_full)
  n_in_gamut <- length(cand_hex_full)
  if (n_in_gamut < n) stop("Too few in-gamut candidates; widen ranges or increase n_candidates.")

  # helper: apply repel to a hex vector and return filtered hex
  apply_repel <- function(hex_vec, repel_deg) {
    if (!length(repel_from) || repel_deg <= 0) return(hex_vec)
    cand_H <- farver::decode_colour(hex_vec, to = "hcl")[, 1]
    keep_mask <- rep(TRUE, length(hex_vec))
    rf <- match.arg(repel_from, c("background","base"), several.ok = TRUE)
    if ("background" %in% rf && is.finite(bg_h)) {
      keep_mask <- keep_mask & (circ_dist(cand_H, bg_h) >= repel_deg)
    }
    if ("base" %in% rf && is.finite(base_h)) {
      keep_mask <- keep_mask & (circ_dist(cand_H, base_h) >= repel_deg)
    }
    hex_vec[keep_mask]
  }

  # try multiple repel levels before relaxing contrast
  repel_grid <- if (length(repel_from)) c(repel_band, seq(max(10, repel_band - 10), 10, by = -10)) else 0

  found <- FALSE
  cand_hex <- NULL
  used_repel <- 0
  used_bg <- contrast_bg
  used_base <- contrast_base

  for (rb in unique(repel_grid)) {
    cand_hex0 <- apply_repel(cand_hex_full, rb)
    if (!length(cand_hex0)) next

    # contrast ladders (bg mandatory; base optional). Interpreted by active backend.
    bg_steps   <- unique(c(contrast_bg, 7.0, 5.0, 4.5, 4.2, 4.0, 3.8, 3.5))
    base_steps <- if (contrast_base > 0) unique(c(contrast_base, 4.5, 3.2, 3.0, 2.8, 2.6, 2.4)) else 0

    ok_here <- FALSE
    for (cb in base_steps) {
      for (cg in bg_steps) {
        keep_bg <- vapply(cand_hex0, function(hx) wcag_contrast_one(hx, background) >= cg, logical(1))
        if (cb > 0) {
          keep_base <- vapply(cand_hex0, function(hx) wcag_contrast_one(hx, base_color) >= cb, logical(1))
          keep <- keep_bg & keep_base
        } else {
          keep <- keep_bg
        }
        if (sum(keep) >= n) {
          cand_hex <- cand_hex0[keep]
          used_repel <- rb
          used_bg <- cg
          used_base <- cb
          ok_here <- TRUE
          break
        }
      }
      if (ok_here) break
    }
    if (ok_here) { found <- TRUE; break }
  }

  if (!found) {
    stop("No candidates pass contrast filters even after relaxation.\n",
         "Try widening hcl ranges or lowering thresholds.")
  }

  # update effective thresholds for info
  contrast_bg_eff   <- used_bg
  contrast_base_eff <- used_base
  if (!quiet && (used_repel != repel_band || used_bg != contrast_bg || used_base != contrast_base)) {
    message(sprintf(
      "Effective settings: repel_band = %d°, contrast_bg = %.1f, contrast_base = %.1f",
      used_repel, used_bg, used_base
    ))
  }

  # --- repel hues near bg/base ----------------------------------------------
  if (length(repel_from)) {
    cand_H <- farver::decode_colour(cand_hex, to = "hcl")[, 1]
    keep_mask <- rep(TRUE, length(cand_hex))
    repel_from <- match.arg(repel_from, c("background","base"), several.ok = TRUE)
    if ("background" %in% repel_from && is.finite(bg_h)) {
      keep_mask <- keep_mask & (circ_dist(cand_H, bg_h) >= repel_band)
    }
    if ("base" %in% repel_from && is.finite(base_h)) {
      keep_mask <- keep_mask & (circ_dist(cand_H, base_h) >= repel_band)
    }
    cand_hex <- cand_hex[keep_mask]
    if (!length(cand_hex)) stop("All candidates filtered by repel bands; reduce `repel_band`.")
  }

  # --- contrast filter (bg mandatory; base optional) -------------------------
  cand_hex0 <- cand_hex
  found <- FALSE
  bg_steps   <- unique(c(contrast_bg, 7.0, 5.0, 4.5, 4.2, 4.0, 3.8, 3.5))
  base_steps <- if (contrast_base > 0) unique(c(contrast_base, 4.5, 3.2, 3.0, 2.8, 2.6, 2.4)) else 0

  for (cb in base_steps) {
    for (cg in bg_steps) {
      keep_bg <- vapply(cand_hex0, function(hx) wcag_contrast_one(hx, background) >= cg, logical(1))
      if (cb > 0) {
        keep_base <- vapply(cand_hex0, function(hx) wcag_contrast_one(hx, base_color) >= cb, logical(1))
        keep <- keep_bg & keep_base
      } else {
        keep <- keep_bg
      }
      if (sum(keep) >= n) {
        cand_hex <- cand_hex0[keep]
        contrast_bg_eff   <- cg
        contrast_base_eff <- cb
        found <- TRUE; break
      }
    }
    if (found) break
  }
  if (!found) {
    stop("No candidates pass contrast filters even after relaxation.\n",
         "Try widening hcl ranges, reducing repel_band, or lowering thresholds.")
  } else if (!quiet && (contrast_base_eff != contrast_base || contrast_bg_eff != contrast_bg)) {
    message(sprintf("Relaxed contrasts used: base = %.1f, background = %.1f",
                    contrast_base_eff, contrast_bg_eff))
  }

  # --- ΔE/hue-spread selection ----------------------------------------------
  rgb255 <- t(grDevices::col2rgb(cand_hex))
  lab    <- farver::convert_colour(rgb255, from = "rgb", to = "lab")
  cand_H <- farver::decode_colour(cand_hex, to = "hcl")[, 1]

  DE_pair <- function(i, j) farver::compare_colour(
    lab[i, , drop = FALSE], lab[j, , drop = FALSE],
    from_space = "lab", method = "CIE2000")[1]

  contr_bg_vals <- vapply(cand_hex, function(hx) wcag_contrast_one(hx, background), numeric(1))
  chosen <- which.max(contr_bg_vals)

  while (length(chosen) < n) {
    left <- setdiff(seq_along(cand_hex), chosen)
    if (!length(left)) break

    minde <- sapply(left, function(i) min(sapply(chosen, function(j) DE_pair(i, j))))
    hueok <- sapply(left, function(i) {
      all(sapply(chosen, function(j) {
        d <- abs(cand_H[i] - cand_H[j]) %% 360
        min(d, 360 - d) >= cur_min_hsep
      }))
    })
    eligible <- left[minde >= cur_min_deltaE & hueok]

    if (!length(eligible)) {
      if (!relax) break
      if (cur_min_hsep > 12) { cur_min_hsep <- cur_min_hsep - 5; if (!quiet) message("Relax: min_hue_sep -> ", cur_min_hsep); next }
      if (cur_min_deltaE > 26){ cur_min_deltaE <- cur_min_deltaE - 2; if (!quiet) message("Relax: min_deltaE -> ",  cur_min_deltaE); next }
      eligible <- left[order(minde, decreasing = TRUE)][1]
    }

    best <- eligible[ which.max( sapply(eligible, function(i) min(sapply(chosen, function(j) DE_pair(i, j)))) ) ]
    chosen <- c(chosen, best)
  }

  res <- cand_hex[chosen]
  attr(res, "info") <- list(
    contrast_bg_eff       = contrast_bg_eff,
    contrast_base_eff     = contrast_base_eff,
    min_deltaE_eff        = cur_min_deltaE,
    min_hue_sep_eff       = cur_min_hsep,
    n_candidates_in_gamut = n_in_gamut,
    n_after_contrast      = length(cand_hex),
    n_selected            = length(res),
    hue_mode              = mode_used,
    repel_band            = repel_band,
    auto_bias             = auto_bias,
    contrast_method       = if (identical(contrast_method, "auto")) selected_backend
    else getOption("ggtwotone.contrast_method", "WCAG")
  )
  res
}

#' Inspect effective palette constraints
#' @param pal A palette returned by highlight_colors()
#' @return A named list of effective thresholds and counts.
#' @export
highlight_info <- function(pal) attr(pal, "info")
