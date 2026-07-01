# Getting Started with ggtwotone

``` r

library(ggtwotone)
library(ggplot2)
```

## Introduction

`ggtwotone` extends **ggplot2** with dual-stroke and contrast-aware
geoms designed to improve the visibility of graphical annotations on
complex backgrounds.

The package is particularly useful for scientific figures containing
images, maps, microscopy data, and heatmaps, where standard single-color
annotations may become difficult to distinguish.

### Design Philosophy

The package follows three guiding principles:

- Improve visibility across heterogeneous backgrounds.
- Support accessibility through contrast-aware color selection.
- Integrate naturally with existing `ggplot2` workflows.

### Main Functions

| Function | Description |
|----|----|
| [`geom_segment_dual()`](../reference/geom_segment_dual.md) | Dual-stroke line segments |
| [`geom_curve_dual()`](../reference/geom_curve_dual.md) | Dual-stroke curved annotations |
| [`geom_path_dual()`](../reference/geom_path_dual.md) | Dual-stroke paths |
| [`geom_function_dual()`](../reference/geom_function_dual.md) | Dual-stroke mathematical functions |
| [`geom_lm_dual()`](../reference/geom_lm_dual.md) | Dual-tone regression lines |
| [`geom_text_contrast()`](../reference/geom_text_contrast.md) | Contrast-aware text labels |
| [`highlight_colors()`](../reference/highlight_colors.md) | High-contrast highlight palettes |
| [`adjust_contrast_pair()`](../reference/adjust_contrast_pair.md) | Contrast-aware color utilities |

### Typical Applications

`ggtwotone` is particularly useful for:

- microscopy and biomedical imaging;
- thematic maps and spatial visualizations;
- heatmaps;
- annotated photographs;
- scientific figures intended for publication;
- graphics that must remain readable in grayscale.

### Documentation

Detailed examples and complete function documentation are available in:

- the package reference website;
- the individual help pages
  ([`?geom_segment_dual`](../reference/geom_segment_dual.md));
- the package README.

### Citation

If you use `ggtwotone` in published work, please cite the package using

``` r

citation("ggtwotone")
```
