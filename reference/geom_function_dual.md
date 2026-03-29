# Dual-Tone Curved Function Lines

Draws a function (e.g., density or mathematical curve) using
perceptually offset dual-stroke curved line segments.

## Usage

``` r
geom_function_dual(
  fun,
  xlim = c(-3, 3),
  n = 701,
  curvature = 0,
  angle = 90,
  ncp = 5,
  colour1 = NULL,
  colour2 = NULL,
  base_color = NULL,
  contrast = 4.5,
  method_contrast = "WCAG",
  linewidth = 1.2,
  args = list(),
  smooth = TRUE,
  color1 = NULL,
  color2 = NULL,
  alpha = 1,
  ...
)
```

## Arguments

- fun:

  A function to evaluate (e.g., `dnorm`, `dt`).

- xlim:

  Range of x-values to evaluate over (numeric vector of length 2).

- n:

  Number of segments to compute (default: 201).

- curvature, angle, ncp:

  Passed to underlying `geom_curve_dual` segments.

- colour1, colour2:

  Fixed top/bottom stroke colours. If only colour1 given, colour2 is
  derived for contrast. (Aliases color1/color2 also accepted.)

- base_color:

  Optional base color to derive a contrast pair (overrides
  colour1/colour2 if supplied).

- contrast, method_contrast:

  Passed to adjust_contrast_pair() when deriving colors.

- linewidth:

  Stroke width for the top line.

- args:

  List of arguments passed to `fun` (for example, list(df = 1) for
  `dt`).

- smooth:

  Use smooth dual-stroke curves (`geom_path`) instead of segmented
  curves (`geom_curve_dual`). Default is TRUE.

- color1, color2:

  U.S.-spelling aliases for `colour1`/`colour2`. Identical in effect;
  prefer `colour1`/`colour2` in code examples.

- alpha:

  Overall opacity for both strokes (0–1).

- ...:

  Additional arguments passed to
  [`geom_curve_dual()`](geom_curve_dual.md).

## Value

A `ggplot2` layer with curved segments.

## Examples

``` r
library(ggplot2)

base <- ggplot() + xlim(-2.05,2.05)
base +
  geom_function_dual(
  fun = function(x) 0.5 * exp(-abs(x)),
  xlim = c(-2, 2),
  color1 = "#EEEEEE",
  color2 = "#222222",
  linewidth = 1,
  smooth = TRUE
  ) +
  theme_dark()



ggplot() +
  geom_function_dual(
    fun = dnorm,
    xlim = c(-5, 5),
    base_color = "green",
    linewidth = 1,
    smooth = TRUE
  ) +
  geom_function_dual(
    fun = dt,
    args = list(df = 1),
    xlim = c(-5, 5),
    base_color = "brown",
    linewidth = 1,
    smooth = TRUE
  ) +
  theme_dark()

```
