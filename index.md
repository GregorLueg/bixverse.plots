# *bixverse.plots package*

[![r_package](https://img.shields.io/github/r-package/v/GregorLueg/bixverse.plots?label=R_package&color=orange)](https://github.com/GregorLueg/bixverse.plots/blob/main/DESCRIPTION)
[![bixverse.plots status
badge](https://gregorlueg.r-universe.dev/bixverse.plots/badges/version)](https://gregorlueg.r-universe.dev/bixverse.plots)
[![CI](https://github.com/GregorLueg/bixverse.plots/actions/workflows/R-cmd-check.yml/badge.svg)](https://github.com/GregorLueg/bixverse.plots/actions/workflows/R-cmd-check.yml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![pkgdown](https://img.shields.io/badge/pkgdown-website-1b5e9f?logo=github)](https://gregorlueg.github.io/bixverse.plots/)

## What is this?

Plotting helpers for [bixverse](https://github.com/GregorLueg/bixverse).
The parent package already has A LOT of code in it, so the plots live
here. Every function is plain `ggplot2` under the hood: take the plot,
add layers, change the theme, done. Pair it with `bixverse` `0.5.x` for
the single cell side.

## What’s in the box?

| Area | Functions |
|----|----|
| Single cell QC | [`violin_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/violin_plot_sc.md), [`density_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/density_plot_sc.md), [`joint_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/joint_plot_sc.md) |
| Embeddings | [`embedding_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/embedding_plot_sc.md), [`feature_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/feature_plot_sc.md), [`label_centroids()`](https://gregorlueg.github.io/bixverse.plots/reference/label_centroids.md) |
| Markers per cluster | [`dot_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/dot_plot_sc.md), [`stacked_violin_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/stacked_violin_plot_sc.md), [`heatmap_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/heatmap_plot_sc.md) |
| Feature pairs, trajectories | [`feature_scatter_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/feature_scatter_plot_sc.md), [`paga_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/paga_plot_sc.md) |
| Differential abundance | [`milo_nhood_plot_sc()`](https://gregorlueg.github.io/bixverse.plots/reference/milo_nhood_plot_sc.md) |
| On/off matrices (SCENIC) | [`plot_binary_heatmap()`](https://gregorlueg.github.io/bixverse.plots/reference/plot_binary_heatmap.md) |
| Gene set enrichment | [`plot_gsea_enrichment()`](https://gregorlueg.github.io/bixverse.plots/reference/plot_gsea_enrichment.md), [`plot_gse_dotplot()`](https://gregorlueg.github.io/bixverse.plots/reference/plot_gse_dotplot.md), [`plot_blitzgsea_null()`](https://gregorlueg.github.io/bixverse.plots/reference/plot_blitzgsea_null.md), [`plot_blitzgsea_es_null()`](https://gregorlueg.github.io/bixverse.plots/reference/plot_blitzgsea_es_null.md) |
| Enrichment maps | [`enrichment_map_gsea()`](https://gregorlueg.github.io/bixverse.plots/reference/enrichment_map_gsea.md), [`enrichment_map_oae()`](https://gregorlueg.github.io/bixverse.plots/reference/enrichment_map_oae.md), [`plot_enrichment_map_ggraph()`](https://gregorlueg.github.io/bixverse.plots/reference/plot_enrichment_map_ggraph.md), [`plot_enrichment_map_visnetwork()`](https://gregorlueg.github.io/bixverse.plots/reference/plot_enrichment_map_visnetwork.md) |
| Differential expression | [`volcano_plot()`](https://gregorlueg.github.io/bixverse.plots/reference/volcano_plot.md) |
| Styling and saving | [`theme_bx()`](https://gregorlueg.github.io/bixverse.plots/reference/theme_bx.md), [`scale_color_bx()`](https://gregorlueg.github.io/bixverse.plots/reference/scale_color_bx.md), [`scale_fill_bx()`](https://gregorlueg.github.io/bixverse.plots/reference/scale_fill_bx.md) (+ `_c` variants), [`save_plot()`](https://gregorlueg.github.io/bixverse.plots/reference/save_plot.md), [`save_plot_ls()`](https://gregorlueg.github.io/bixverse.plots/reference/save_plot_ls.md) |

Want to see them in action? The single cell plots get their tour in the
[bixverse single cell visualisation
vignette](https://gregorlueg.github.io/bixverse/articles/single_cell_visualisation.html).
Gene set enrichment has vignettes here:
[GSEA](https://gregorlueg.github.io/bixverse.plots/articles/gsea_visualisation.html)
(fgsea and blitzGSEA) and
[over-representation](https://gregorlueg.github.io/bixverse.plots/articles/ora_visualisation.html).
What changed between versions is in
[NEWS](https://gregorlueg.github.io/bixverse.plots/NEWS.md).

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

*Last update to the read-me: 01.10.2026*
