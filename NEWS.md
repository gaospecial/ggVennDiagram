# ggVennDiagram 1.0

* We completely rebuild `ggVennDiagram` in v1.0. It is now has well-defined data structure and maybe more elegant in coding.
* Although all of the main functions are retained, the usage of a few functions has been modified. This may cause capacity issues.
* If you find a bug or have questions, feel free to open a issue in [GitHub](https://github.com/gaospecial/ggVennDiagram/issues).

## Features

* user can modify the edge/label just use ordinary ggplot functions, without extra knowledge.
* support up to 7 sets Venn diagram. We thank the author of `venn` providing required edge shape data.
* less memory usage and maybe more faster.
* a lot of helper functions.

## Changes

* if you set `show_intersect = TRUE` in the `ggVennDiagram()` function, it will now return a html widget directly by `plotly`. 
* several parameters are removed from the main function `ggVennDiagram()`, including `lty`, `color`, and so on. But these settings can be easily accessed with ggplot functions. See [README](./README.md).

# ggVennDiagram 0.5

* Intersection values can be obtained and visualized now. See [README](./README.md) for more information;

* Bug fixings.
