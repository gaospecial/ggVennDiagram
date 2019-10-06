
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggVennDiagram

<!-- badges: start -->

<!-- badges: end -->

‘`ggVennDiagram`’ enables fancy venn plot with 2-4 sets and generates
publication quality figure. It is the first software that can
automatically fill different colors to each part of a venn diagram.

## Installation

You can install the released version of ggVennDiagram from
[CRAN](https://CRAN.R-project.org) with (under evaluation in CRAN):

``` r
install.packages("ggVennDiagram")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gaospecial/ggVennDiagram")
```

## Example

`ggVennDiagram` maps the fill color of each region to quantity, allowing
us to visually observe the differences between different parts.

``` r
library(ggVennDiagram)
genes <- paste("gene",1:1000,sep="")
set.seed(20190708)
x <- list(A=sample(genes,300),B=sample(genes,525),C=sample(genes,440),D=sample(genes,350))

# four dimension venn plot
ggVennDiagram(x)
```

<img src="man/figures/README-example-1.png" width="70%" />

``` r

# three dimension venn plot
ggVennDiagram(x[1:3])
```

<img src="man/figures/README-example-2.png" width="70%" />

``` r

# two dimension venn plot
ggVennDiagram(x[1:2])
```

<img src="man/figures/README-example-3.png" width="70%" />

`ggVennDiagram` return a ggplot object, which can be further modified
with `ggplot` functions.

``` r
library(ggplot2)
ggVennDiagram(x) + scale_fill_gradient(low="blue",high = "red")
#> Scale for 'fill' is already present. Adding another scale for 'fill',
#> which will replace the existing scale.
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="70%" />

``` r

ggVennDiagram(x,lty="dashed",color="black",size=2) + scale_fill_gradient(low="white",high = "red")
#> Scale for 'fill' is already present. Adding another scale for 'fill',
#> which will replace the existing scale.
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="70%" />

`ggVennDiagram` now support 2-4 dimension venn plot. The generated
figure is generally ready for publish. The main function
`ggVennDiagram()` will check how many items in the first paramenter and
call corresponding function automatically.

The parameter `category.names` reprents set names. And the parameter
`label` can label how many items are included in each parts.

``` r
ggVennDiagram(x,category.names = c("Stage 1","Stage 2","Stage 3", "Stage4"))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="70%" />

``` r

ggVennDiagram(x,category.names = c("Stage 1","Stage 2","Stage 3", "Stage4"), label = NULL)
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="70%" />
