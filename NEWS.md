# Changelog of ggVennDiagram

## ggVennDiagram 1.5.6

* Add a param `sets.bar.position` (default is "left", can be set to "right") to allow left side layout of barplot in `plot_upset()`. See [#78](https://github.com/gaospecial/ggVennDiagram/issues/78).

## ggVennDiagram 1.5.3

* Add a param to allow user specifying shape by id. See #60.

## ggVennDiagram 1.5.2

* Add a switch to show non-specific items in `plot_upset`. See #64.

## ggVennDiagram 1.5.1

* Add more param to `plot_upset()` function
* Add vignette of Venn Calculator

## ggVennDiagram 1.5.0

* This is a CRAN release

## ggVennDiagram 1.4.8

* add native support to upset plot. Upset plot will be automatically called when nsets > 7. Besides, it can be used by using `upset_plot()` function. See `?upset_plot` for more information.
* a simple shiny app. Run `launch_app()` to use ggVennDiagram interactively.

## ggVennDiagram 1.4

* remove the dependency of `sf`, `purrr`, etc. It is now easy to install.
* add a `print()` method for VennPlotData object.
* add a `show()` method for Venn object.
* `VennPlotData` now store shape and datasets in ordinary data.frames. See `?VennPlotData` for more information.
* many other optimization.

## ggVennDiagram 1.2.3

* Minor fixing for ggplot 3.4

## ggVennDiagram 1.2.2

* I removed the package dependency to `stringr`

## ggVennDiagram 1.2

* Paper published. doi: 10.3389/fgene.2021.706907
* minor fixing of URL
* minor changes

## ggVennDiagram 1.1.4

* minor changes and bug fixing

## ggVennDiagram 1.1.3

* we now export more internal functions which are used in internal data process,
this allow the users to use novel shapes with there data.
* the vignette has benn split to three by topics, and a new vignette was added.
* document has been updated, including pkgdown settings.

## ggVennDiagram 1.1.2

* setup GitHub actions
* enable GitHub pages: https://gaospecial.github.io/ggVennDiagram/index.html
* setup Code Coverage analysis

## ggVennDiagram 1.1.1

* add pkgdown docments

## ggVennDiagram 1.1

* more handy parameters were added to `ggVennDiagram()`, make it much easier to set edge/setlabel/regionlabel color and size.
* some default parameter was reset to previous values in consideration of compatible.
* we released a online book to show detailed usage at https://venn.bio-spring.info.

## ggVennDiagram 1.0

* We completely rebuild `ggVennDiagram` in v1.0. It is now has well-defined data structure and maybe more elegant in coding.
* Although all of the main functions are retained, the usage of a few functions has been modified. This may cause capacity issues.
* If you find a bug or have questions, feel free to open a issue in [GitHub](https://github.com/gaospecial/ggVennDiagram/issues).

### Features

* user can modify the edge/label just use ordinary ggplot functions, without extra knowledge.
* support up to 7 sets Venn diagram, based on the polygon data imported from package `venn`, which is another Venn plot tool authored by Adrian Dusa.
* less memory usage and maybe more faster.
* a lot of helper functions.

### Changes

* if you set `show_intersect = TRUE` in the `ggVennDiagram()` function, it will now return a html widget directly by `plotly`. 
* several parameters are removed from the main function `ggVennDiagram()`, including `lty`, `color`, and so on. But these settings can be easily accessed with ggplot functions. See [README](./README.md).

## ggVennDiagram 0.5

* Intersection values can be obtained and visualized now. See [README](./README.md) for more information;

* Bug fixings.
