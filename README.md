
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Dynamic Zoning

<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of dz is to showcase algorithms developed to handle dynamic
zoning applications for the team-orienteering problem.

## Installation

You can install the development version of dz from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Rosenkrands/dz")
```

We can then load the library with the following command:

``` r
library(dz)
```

## Test instances

There are 7 seven test instances included in the package. They can be
accessed with `dz::test_instances`.

``` r
length(test_instances)
#> [1] 7
```

``` r
inst <- test_instances$p7_chao 
plot(inst, delaunay = T, voronoi = F)
```

<img src="man/figures/README-plot_test_instance-1.png" width="100%" />

## Clustering

With the `clustering` function we are able to decompose an instance into
a number of disjoint sets (disregarding the source node).

As of now there are two methods to perform the clustering; the greedy
approach and a local search approach. Below we can see the resulting
clusters from the greedy approach.

<img src="man/figures/README-perform_clustering_gr-1.png" width="100%" />

The local search approach tries to improve on the greedy approach using
an insertion operator. Resulting clusters from the local search approach
are shown below.

<img src="man/figures/README-perform_clustering_ls-1.png" width="100%" />

In the below animation we can see how the initial clustering in
iteratively improved using the local search approach.

![](local_search_animation.gif)

The animation is created using `animate_local_search(clust_ls)`.

## Routing

Given an instance with clusters we are able to find a set of routes
using the `routing` function.

``` r
rout <- routing(
  clust = clust_ls,
  obj = "SDR",
  L = 500
)

plot(rout)
```

<img src="man/figures/README-perform_routing-1.png" width="100%" />
