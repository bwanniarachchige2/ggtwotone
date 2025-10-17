GeomTextContrast <- ggplot2::ggproto("GeomTextContrast", ggplot2::Geom,
                                     required_aes = c("x", "y", "label"),   # background can be param OR aes
                                     default_aes = ggplot2::aes(
                                       size = 3.88, angle = 0,
                                       hjust = 0.5, vjust = 0.5,
                                       alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                                       background = NA     # allow as aesthetic (optional)
                                       #base_colour = NA      # allow as aesthetic (optional)
                                     ),
                                     draw_key = ggplot2::draw_key_text,

                                     draw_panel = function(self, data, panel_params, coord,
                                                           parse = FALSE, na.rm = FALSE,
                                                           method = "auto", contrast = 4.5,
                                                           background = NULL,
                                                           base_colour = NULL) {

                                       # coalesce naming
                                       if (is.null(base_colour) && !is.null(base_colour)) base_colour <- base_colour

                                       if (!requireNamespace("colorspace", quietly = TRUE))
                                         stop("Package 'colorspace' is required for contrast-aware text.")
                                       if (!exists("adjust_contrast_pair", mode = "function"))
                                         stop("`adjust_contrast_pair()` must exist in your namespace.")

                                       # recycle-safe accessor for a vector param
                                       get_from_param <- function(vec, i) {
                                         if (is.null(vec) || length(vec) == 0) return(NA_character_)
                                         j <- ((i - 1) %% length(vec)) + 1
                                         vec[j]
                                       }

                                       bg_is_light <- function(bg) {
                                         colorspace::contrast_ratio(bg, "#000000") >= colorspace::contrast_ratio(bg, "#FFFFFF")
                                       }

                                       n <- nrow(data)
                                       data$colour <- vapply(seq_len(n), function(i) {
                                         # --- background: aes > param ---
                                         bg_i <- data$background[i]
                                         if (is.na(bg_i) || is.null(bg_i)) bg_i <- get_from_param(background, i)
                                         if (is.na(bg_i) || is.null(bg_i)) bg_i <- "#FFFFFF"  # last-resort safe default

                                         # --- base: aes > param > default ---
                                         base_i <- data$base_colour[i]
                                         if (is.na(base_i) || is.null(base_i)) base_i <- base_colour
                                         if (is.na(base_i) || is.null(base_i)) base_i <- "#666666"

                                         # derive candidates
                                         cp <- tryCatch(
                                           adjust_contrast_pair(
                                             base_i, contrast = contrast, method = method, background = bg_i
                                           ),
                                           error = function(e) list(light = "#FFFFFF", dark = "#000000")
                                         )
                                         light <- cp$light; dark <- cp$dark

                                         crL <- suppressWarnings(colorspace::contrast_ratio(light, bg_i))
                                         crD <- suppressWarnings(colorspace::contrast_ratio(dark,  bg_i))
                                         prefer_dark <- bg_is_light(bg_i)

                                         pick <- NULL
                                         if (prefer_dark) {
                                           if (is.finite(crD) && crD >= contrast) pick <- dark
                                           else if (is.finite(crL) && crL >= contrast) pick <- light
                                         } else {
                                           if (is.finite(crL) && crL >= contrast) pick <- light
                                           else if (is.finite(crD) && crD >= contrast) pick <- dark
                                         }
                                         if (is.null(pick)) {
                                           if (is.finite(crL) && is.finite(crD)) pick <- if (crL >= crD) light else dark
                                           else if (is.finite(crL)) pick <- light
                                           else if (is.finite(crD)) pick <- dark
                                           else pick <- if (colorspace::contrast_ratio("#FFFFFF", bg_i) >=
                                                            colorspace::contrast_ratio("#000000", bg_i)) "#FFFFFF" else "#000000"
                                         }
                                         pick
                                       }, character(1))

                                       coords <- coord$transform(data, panel_params)
                                       alpha_val <- if (all(is.na(coords$alpha))) NULL else coords$alpha

                                       grid::textGrob(
                                         label = coords$label,
                                         x = coords$x, y = coords$y,
                                         default.units = "native",
                                         hjust = coords$hjust, vjust = coords$vjust,
                                         rot = coords$angle,
                                         gp = grid::gpar(
                                           col = coords$colour,
                                           fontsize = coords$size * ggplot2::.pt,
                                           fontface = coords$fontface,
                                           fontfamily = coords$family,
                                           lineheight = coords$lineheight,
                                           alpha = alpha_val
                                         )
                                       )
                                     }
)

#' Contrast-Aware Text Geom
#'
#' Automatically adjusts text colour for readability on varying background colours using WCAG or APCA contrast.
#'
#' @rdname geom_text_contrast
#' @inheritParams ggplot2::geom_text
#' @param background A character vector of background fill colours (hex codes), used for contrast computation.
#' @param base_colour Base text colour used to generate light/dark variants.
#' @param method Contrast method to use: `"WCAG"`, `"APCA"`, or `"auto"`.
#' @param contrast Threshold to ensure between text and background (defaults to 4.5).
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Grayscale A–E tiles: testing contrast
#' df <- data.frame(
#'   x = 1:5,
#'   y = 1,
#'   label = LETTERS[1:5],
#'   fill = c("#000000", "#222222", "#666666", "#DDDDDD", "#FFFFFF")
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = fill), width = 1, height = 1) +
#'   geom_text_contrast(
#'     aes(label = label),
#'     background = df$fill,
#'     size = 7
#'   ) +
#'   scale_fill_identity() +
#'   coord_fixed() +
#'   theme_void() +
#'   labs(title = "Contrast-Aware Text on Varying Backgrounds")
#'
#' library(dplyr)
#' library(scales)
#'
#' set.seed(1)
#' classes <-c("Cat", "Dog", "Rabit")
#' cm <- expand.grid(True = classes, Predicted = classes)
#' cm$Count <- sample(5:200, size = nrow(cm), replace = TRUE)
#'
#' cm <- cm %>%
#'   group_by(True) %>%
#'   mutate(Accuracy = Count / sum(Count))
#'
#' pal <- c("#313695", "#74add1", "#fdae61", "#a50026")
#' col_fun <- col_numeric(palette = pal, domain = c(0, 1))
#'
#' cm$fill_hex <- col_fun(cm$Accuracy)
#' cm$label <- sprintf("%.1f%%", 100 * cm$Accuracy)
#'
#' ggplot(cm, aes(Predicted, True)) +
#'   geom_tile(aes(fill = Accuracy), color = "white", linewidth = 0.8) +
#'     geom_text_contrast(
#'     aes(label = label),
#'     background = cm$fill_hex,
#'     base_colour = "#004488",
#'     method = "auto",
#'     contrast = 4.5,
#'     size = 5, fontface = "bold"
#'   ) +
#'   scale_fill_gradientn(
#'     colours = pal,
#'     limits = c(0, 1),
#'     name = "Accuracy"
#'   ) +
#'   coord_fixed() +
#'   labs(
#'     title = "Confusion Matrix with Auto-Contrast Labels",
#'     x = "Predicted Class", y = "True Class"
#'   ) +
#'   theme_minimal(base_size = 13) +
#'   theme(
#'     panel.grid = element_blank(),
#'     axis.text.x = element_text(angle = 45, hjust = 1)
#'   )
#'
#'
#' # Simulated region × risk category
#' df_risk <- expand.grid(
#'   region = LETTERS[1:6],
#'   zone = paste0("Z", 1:6)
#' )
#'
#' df_risk$risk_level <- sample(
#'   c("Low", "Moderate", "High", "Critical", "Severe", "Extreme"),
#'   size = nrow(df_risk), replace = TRUE
#' )
#' df_risk$label <- paste(df_risk$region, df_risk$zone)
#'
#' risk_colours <- c(
#'   "Low" = "gray80",
#'   "Moderate" = "skyblue",
#'   "High" = "orange",
#'   "Critical" = "firebrick",
#'   "Severe" = "darkred",
#'   "Extreme" = "navy"
#' )
#'
#' df_risk$fill_colour <- risk_colours[df_risk$risk_level]
#'
#' ggplot(df_risk, aes(x = region, y = zone, fill = risk_level)) +
#'   geom_tile(colour = "white") +
#'   geom_text_contrast(
#'     aes(label = label),
#'     background = df_risk$fill_colour,
#'     size = 3,
#'     fontface = "bold"
#'   ) +
#'   scale_fill_manual(values = risk_colours) +
#'   labs(
#'     title = "Simulated Risk Map (Auto Contrast Labels)",
#'     fill = "Risk Level"
#'   ) +
#'   theme_minimal()
geom_text_contrast <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", ...,
                               method = "auto", contrast = 4.5,
                               base_colour = NULL,
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, background = NULL) {

  layer(
    geom = GeomTextContrast, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method,
      contrast = contrast,
      base_colour = base_colour,
      background = background,
      na.rm = na.rm,
      ...
    )
  )
}



