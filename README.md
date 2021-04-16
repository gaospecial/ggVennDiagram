
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggVennDiagram

<!-- badges: start -->
<!-- badges: end -->

‘`ggVennDiagram`’ enables fancy Venn plot with 2-4 sets and generates
publication quality figure. It is the first software that can
automatically fill different colors to each part of a Venn diagram.

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
#> Loading required package: RVenn
genes <- paste("gene",1:1000,sep="")
set.seed(20190708)
x <- list(A=sample(genes,300),B=sample(genes,525),C=sample(genes,440),D=sample(genes,350))

# four dimension Venn plot
ggVennDiagram(x)
```

<img src="man/figures/README-example-1.png" width="70%" />

``` r
# three dimension Venn plot
ggVennDiagram(x[1:3])
```

<img src="man/figures/README-example-2.png" width="70%" />

``` r
# two dimension Venn plot
ggVennDiagram(x[1:2])
```

<img src="man/figures/README-example-3.png" width="70%" />

`ggVennDiagram` return a `ggplot` object, which can be further modified
with `ggplot` functions.

``` r
library(ggplot2)
ggVennDiagram(x) + scale_fill_gradient(low="blue",high = "red")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="70%" />

``` r
ggVennDiagram(x,lty="dashed",color="black",size=2) + scale_fill_gradient(low="white",high = "red")
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="70%" />

`ggVennDiagram` now support 2-4 dimension Venn plot. The generated
figure is generally ready for publish. The main function
`ggVennDiagram()` will check how many items in the first parameter and
call corresponding function automatically.

The parameter `category.names` is set names. And the parameter `label`
can label how many items are included in each parts.

``` r
ggVennDiagram(x,category.names = c("Stage 1","Stage 2","Stage 3", "Stage4"))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="70%" />

``` r
ggVennDiagram(x,category.names = c("Stage 1","Stage 2","Stage 3", "Stage4"), label = NULL)
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="70%" />

Set `label_alpha = 0` to remove label background

``` r
ggVennDiagram(x, label_alpha=0)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="70%" />

## Showing intersection values

*Note: you need to install the GitHub version to enable these
functions.*

We implemented the `process_region_data()` to get intersection values.

``` r
y <- list(
  A = sample(letters, 8),
  B = sample(letters, 8),
  C = sample(letters, 8),
  D = sample(letters, 8)
)

process_region_data(Venn(y))
#> # A tibble: 15 x 5
#>    component id    item      count name      
#>    <chr>     <chr> <list>    <int> <chr>     
#>  1 region    1     <chr [3]>     3 A         
#>  2 region    2     <chr [2]>     2 B         
#>  3 region    3     <chr [1]>     1 C         
#>  4 region    4     <chr [1]>     1 D         
#>  5 region    12    <chr [1]>     1 A..B      
#>  6 region    13    <chr [0]>     0 A..C      
#>  7 region    14    <chr [1]>     1 A..D      
#>  8 region    23    <chr [1]>     1 B..C      
#>  9 region    24    <chr [1]>     1 B..D      
#> 10 region    34    <chr [2]>     2 C..D      
#> 11 region    123   <chr [1]>     1 A..B..C   
#> 12 region    124   <chr [0]>     0 A..B..D   
#> 13 region    134   <chr [1]>     1 A..C..D   
#> 14 region    234   <chr [1]>     1 B..C..D   
#> 15 region    1234  <chr [1]>     1 A..B..C..D
```

If only several items were included, intersections may also be viewed
interactively by `plotly` method (if you have two many items, this is
useless).

``` r
ggVennDiagram(y, show_intersect = TRUE)
```

In web browser or RStudio, you will get:

<img src="https://vnote-1251564393.cos.ap-chengdu.myqcloud.com/typora-img/intersection.gif" width="70%" />

# Venn Diagram for more than four sets

If you have reviewed my codes, you may find it is easy to support Venn
Diagram for more than four sets, as soon as you find a ideal parameter
to generate more circles or ellipses in the plot. The key point is to
let the generated ellipses have exactly one intersection for each
combination.

However, Venn Diagram for more than four sets may be meaningless in some
conditions, as some parts may be omitted in such ellipses. Therefore, it
is only useful in specific conditions. For example, if the set
intersection of all group are extremely large, you may use several
ellipses to draw a “flower” to show that.

# “`ggVennDiagram`” 诞生记

在 *@GuangchuangYu* 的公众号下面，我投稿了一篇文章，介绍了
“`ggVennDiagram`”
包开发的始末，有兴趣的同学可以移步[至此](https://mp.weixin.qq.com/s/peNWKC5m7EWEv6w3m4rsIA)查看。
