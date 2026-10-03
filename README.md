# *bixverse.plots package* <img src="man/figures/logo.png" align="right" height="138" alt="bixverse.plots logo" />

[![r_package](https://img.shields.io/github/r-package/v/GregorLueg/bixverse.plots?label=R_package&color=orange)](https://github.com/GregorLueg/bixverse.plots/blob/main/DESCRIPTION)
[![bixverse.plots status badge](https://gregorlueg.r-universe.dev/bixverse.plots/badges/version)](https://gregorlueg.r-universe.dev/bixverse.plots)
[![CI](https://github.com/GregorLueg/bixverse.plots/actions/workflows/R-cmd-check.yml/badge.svg)](https://github.com/GregorLueg/bixverse.plots/actions/workflows/R-cmd-check.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![pkgdown](https://img.shields.io/badge/pkgdown-website-1b5e9f?logo=github)](https://gregorlueg.github.io/bixverse.plots/)

## What is this?

Plotting helpers for [bixverse](https://github.com/GregorLueg/bixverse). The
parent package already has A LOT of code in it, so the plots live here. Every
function is plain `ggplot2` under the hood: take the plot, add layers, change
the theme, done. Pair it with `bixverse` `0.5.x` for the single cell side.

## What's in the box?

| Area | Functions |
|---|---|
| Single cell QC | `violin_plot_sc()`, `density_plot_sc()`, `joint_plot_sc()` |
| Embeddings | `embedding_plot_sc()`, `feature_plot_sc()`, `label_centroids()` |
| Markers per cluster | `dot_plot_sc()`, `stacked_violin_plot_sc()`, `heatmap_plot_sc()` |
| Feature pairs, trajectories | `feature_scatter_plot_sc()`, `paga_plot_sc()` |
| Differential abundance | `milo_nhood_plot_sc()` |
| On/off matrices (SCENIC) | `plot_binary_heatmap()` |
| Gene set enrichment | `plot_gsea_enrichment()`, `plot_gse_dotplot()`, `plot_blitzgsea_null()`, `plot_blitzgsea_es_null()` |
| Enrichment maps | `enrichment_map_gsea()`, `enrichment_map_oae()`, `plot_enrichment_map_ggraph()`, `plot_enrichment_map_visnetwork()` |
| Differential expression | `volcano_plot()` |
| Styling and saving | `theme_bx()`, `scale_color_bx()`, `scale_fill_bx()` (+ `_c` variants), `save_plot()`, `save_plot_ls()` |

Want to see them in action? The single cell plots get their tour in the
[bixverse single cell visualisation vignette](https://gregorlueg.github.io/bixverse/articles/single_cell_visualisation.html).
Gene set enrichment has vignettes here:
[GSEA](https://gregorlueg.github.io/bixverse.plots/articles/gsea_visualisation.html)
(fgsea and blitzGSEA) and
[over-representation](https://gregorlueg.github.io/bixverse.plots/articles/ora_visualisation.html).
What changed between versions is in [NEWS](NEWS.md).

## Installation

r-universe gives you a pre-built binary of both this package and `bixverse`, so
no Rust toolchain and no compile:

```r
install.packages(
  "bixverse.plots",
  repos = c("https://gregorlueg.r-universe.dev", "https://cloud.r-project.org")
)
```

Building from source needs Rust, because `bixverse` does. Check the
[bixverse README](https://github.com/GregorLueg/bixverse) for that set-up. Keep
the r-universe repo in the list either way:

```r
options(repos = c("https://gregorlueg.r-universe.dev", getOption("repos")))
devtools::install_github("https://github.com/GregorLueg/bixverse.plots")
```

*Last update to the read-me: 01.10.2026*
