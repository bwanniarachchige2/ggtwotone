# Dual-Stroke Line Segments with Side-by-Side Offset

Draws two side-by-side line segments with separate colours for improved
visibility on varied backgrounds.

## Usage

``` r
geom_segment_dual(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  colour1 = NULL,
  colour2 = NULL,
  linewidth = NULL,
  lineend = "butt",
  aspect_ratio = 1,
  ...,
  arrow = NULL,
  arrow.fill = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- colour1:

  Colour for one side of the dual stroke.

- colour2:

  Colour for the other side of the dual stroke.

- linewidth:

  Width of the total dual stroke (in mm).

- lineend:

  Line end style (round, butt, square).

- aspect_ratio:

  Aspect ratio hint (currently unused by the grob logic but reserved for
  future layout tuning).

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- arrow:

  specification for arrow heads, as created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- arrow.fill:

  fill colour to use for the arrow head (if closed). `NULL` means use
  `colour` aesthetic.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Details

Dual-Stroke Line Segments with Side-by-Side Offset

Draws two side-by-side line segments with separate colours for improved
visibility on varied backgrounds.

## Examples

``` r
# Simple black background test
ggplot(data.frame(x = 1, xend = 2, y = 1, yend = 2),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment_dual(colour1 = "white", colour2 = "black", linewidth = 2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "gray20"))


# Dual-stroke diagonal lines crossing contrasting backgrounds
bg <- data.frame(
  xmin = c(0, 5),
  xmax = c(5, 10),
  ymin = 0,
  ymax = 5,
  fill = c("black", "white")
)

line_data <- data.frame(
  x = c(1, 9),
  y = c(1, 1),
  xend = c(9, 1),
  yend = c(4, 4),
  colour1 = c("#D9D9D9", "#D9D9D9"),  # light stroke
  colour2 = c("#333333", "#333333")   # dark stroke
)

ggplot() +
  geom_rect(data = bg,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
  inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_segment_dual(
    data = line_data,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour1 = line_data$colour1,
    colour2 = line_data$colour2,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  theme_void() +
  coord_fixed() +
  ggtitle("Two Diagonal Dual-Stroke Lines in Opposite Directions")


# Multiple dual-stroke segments with arrowheads and grouping
df <- data.frame(
  x = c(1, 2, 3),
  xend = c(2, 3, 4),
  y = c(1, 2, 1),
  yend = c(2, 1, 2),
  colour1 = rep("white", 3),
  colour2 = rep("black", 3),
  group = factor(c("A", "B", "C"))
)

ggplot(df) +
  geom_segment_dual(
    aes(x = x, y = y, xend = xend, yend = yend, group = group),
    colour1 = df$colour1,
    colour2 = df$colour2,
    linewidth = 1,
    arrow = arrow(length = unit(0.15, "inches"), type = "closed")
  ) +
  coord_fixed() +
  theme_dark()
```
