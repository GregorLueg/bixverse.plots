# *bixverse.plots package*

[![r_package](https://img.shields.io/github/r-package/v/GregorLueg/bixverse.plots?label=R_package&color=orange)](https://github.com/GregorLueg/bixverse.plots/blob/main/DESCRIPTION)
[![bixverse.plots status
badge](https://gregorlueg.r-universe.dev/bixverse.plots/badges/version)](https://gregorlueg.r-universe.dev/bixverse.plots)
[![bixverse.plots status
badge](https://gregorlueg.r-universe.dev/bixverse.plots/badges/version)](https://gregorlueg.r-universe.dev/bixverse.plots)
[![CI](https://github.com/GregorLueg/bixverse.plots/actions/workflows/R-cmd-check.yml/badge.svg)](https://github.com/GregorLueg/bixverse.plots/actions/workflows/R-cmd-check.yml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![pkgdown](https://img.shields.io/badge/pkgdown-website-1b5e9f?logo=github)](https://gregorlueg.github.io/bixverse.plots/)

## Intro

This package contains a lot of additional plotting helpers for the
[bixverse](https://github.com/GregorLueg/bixverse) package. It combines
especially well with `"0.5.x"` versions of `bixverse` as it supports all
of the new single cell functionality. The idea is to not overload the
parent package, as this one has already A LOT of code in it.

## *Release notes*

Official release of the package with **0.2.4**. In this version, we have
introduced a huge number of plotting helpers for single cell, please
refer to this vignette
[here](https://gregorlueg.github.io/bixverse/articles/single_cell_visualisation.html)
for more details.

The gene set enrichment side has vignettes of its own:
[GSEA](https://gregorlueg.github.io/bixverse.plots/articles/gsea_visualisation.html)
(fgsea and blitzGSEA) and
[over-representation](https://gregorlueg.github.io/bixverse.plots/articles/ora_visualisation.html).

## Installation

r-universe gives you a pre-built binary of both this package and
`bixverse`, so no Rust toolchain and no compile:

``` r

install.packages(
  "bixverse.plots",
  repos = c("https://gregorlueg.r-universe.dev", "https://cloud.r-project.org")
)
```

Building from source needs Rust, because `bixverse` does. Check the
[bixverse README](https://github.com/GregorLueg/bixverse) for that
set-up. Keep the r-universe repo in the list either way:

``` r

options(repos = c("https://gregorlueg.r-universe.dev", getOption("repos")))
devtools::install_github("https://github.com/GregorLueg/bixverse.plots")
```

## Aim

Provide additional plotting capabilities to `bixverse`.

*Last update to the read-me: 04.09.2026*
