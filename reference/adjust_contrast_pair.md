# Adjust Contrast Between Two Stroke Colors

Given a base color and a background, generate a pair of colors (light
and dark) with sufficient perceptual contrast using WCAG or APCA
methods.

## Usage

``` r
adjust_contrast_pair(
  color,
  contrast = 4.5,
  method = "auto",
  background = "#FFFFFF",
  quiet = FALSE
)
```

## Arguments

- color:

  A base color, as a hex string or valid R color name (e.g., "#6699CC",
  "darkred").

- contrast:

  Minimum desired contrast ratio (default is 4.5).

- method:

  Contrast method: "WCAG", "APCA", or "auto" to try both.

- background:

  Background color, as a hex string or valid R color name (default:
  "#FFFFFF").

- quiet:

  Logical. If TRUE, suppresses warnings.

## Value

A list with elements `light`, `dark`, `contrast`, and `method`.

## Examples

``` r
adjust_contrast_pair("#777777", contrast = 4.5,
method = "auto", background = "#000000")
#> $light
#> [1] "#303030"
#> 
#> $dark
#> [1] "#F1F1F1"
#> 
#> $contrast
#> [1] 18.59245
#> 
#> $method
#> [1] "WCAG"
#> 

adjust_contrast_pair("#66CCFF", contrast = 4.5,
method = "APCA", background = "#FAFAFA")
#> $light
#> [1] "#9FFCFF"
#> 
#> $dark
#> [1] "#004279"
#> 
#> $contrast
#> [1] 90.11555
#> 
#> $method
#> [1] "APCA"
#> 
```
