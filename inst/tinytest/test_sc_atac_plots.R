# sc atac plotting -------------------------------------------------------------

library(bixverse)

## hand-made tables ------------------------------------------------------------

depth_cor <- data.table::data.table(
  component = 1:5,
  depth_cor = c(-0.95, 0.4, -0.1, 0.05, 0.2),
  dropped = c(TRUE, FALSE, FALSE, FALSE, FALSE)
)

offsets <- seq(-100L, 100L, by = 10L)
tss <- data.table::data.table(
  group = factor(rep(c("a", "b"), each = length(offsets))),
  offset = rep(offsets, 2),
  insertions = rep(100 * exp(-abs(offsets) / 30), 2),
  enrichment = rep(1 + 5 * exp(-abs(offsets) / 30), 2)
)

lens <- 1:500
lengths <- data.table::data.table(
  group = factor(rep(c("a", "b"), each = length(lens))),
  length = rep(lens, 2),
  fragments = rep(round(1000 * exp(-lens / 100)), 2)
)
lengths[, fraction := fragments / sum(fragments), by = group]

### lsi depth plot -------------------------------------------------------------

p <- lsi_depth_plot_sc(depth_cor, threshold = 0.5)
expect_true(
  checkmate::checkClass(p, "ggplot"),
  info = "lsi_depth_plot_sc: correct class"
)
expect_equal(
  sort(ggplot2::layer_data(p, 1)$yintercept),
  c(-0.5, 0.5),
  info = "lsi_depth_plot_sc: threshold guides"
)
expect_equal(
  nrow(ggplot2::layer_data(p, 3)),
  4L,
  info = "lsi_depth_plot_sc: kept components filled"
)
expect_equal(
  ggplot2::layer_data(p, 4)$shape,
  1,
  info = "lsi_depth_plot_sc: dropped component hollow"
)

p <- lsi_depth_plot_sc(depth_cor[, .(component, depth_cor)])
expect_equal(
  nrow(ggplot2::layer_data(p, 4)),
  0L,
  info = "lsi_depth_plot_sc: no dropped column, nothing hollow"
)

expect_error(
  lsi_depth_plot_sc(depth_cor[, .(component)]),
  info = "lsi_depth_plot_sc: missing column"
)

### tss profile plot -----------------------------------------------------------

p <- tss_profile_plot_sc(tss)
expect_true(
  checkmate::checkClass(p, "ggplot"),
  info = "tss_profile_plot_sc: correct class"
)
expect_equal(
  p$labels$y,
  "TSS enrichment",
  info = "tss_profile_plot_sc: enrichment label"
)
expect_equal(
  length(unique(ggplot2::layer_data(p, 1)$colour)),
  2L,
  info = "tss_profile_plot_sc: one line per group"
)

p <- tss_profile_plot_sc(tss, value = "insertions")
expect_equal(
  ggplot2::layer_data(p, 1)$y[ggplot2::layer_data(p, 1)$x == 0][1],
  100,
  info = "tss_profile_plot_sc: insertions on y"
)

p <- tss_profile_plot_sc(tss[group == "a"])
expect_equal(
  p$theme$legend.position,
  "none",
  info = "tss_profile_plot_sc: no legend with one group"
)

expect_error(
  tss_profile_plot_sc(tss[, .(group, offset)]),
  info = "tss_profile_plot_sc: missing column"
)

### fragment length plot -------------------------------------------------------

p <- fragment_length_plot_sc(lengths)
expect_true(
  checkmate::checkClass(p, "ggplot"),
  info = "fragment_length_plot_sc: correct class"
)
expect_equal(
  ggplot2::layer_data(p, 2)$xintercept,
  c(147, 294),
  info = "fragment_length_plot_sc: nucleosome guides"
)

p <- fragment_length_plot_sc(lengths, log_scale = TRUE)
expect_true(
  p$scales$has_scale("y"),
  info = "fragment_length_plot_sc: log y scale"
)

expect_error(
  fragment_length_plot_sc(lengths[, .(group, length)]),
  info = "fragment_length_plot_sc: missing column"
)

## real object -----------------------------------------------------------------

# chr22 PBMC fixtures from bixverse-rs, see bixverse's
# inst/tinytest/helper_atac.R
atac_fix <- Sys.getenv(
  "BIXVERSE_ATAC_FIXTURES",
  path.expand(
    "~/repos/shared/bixverse-rs/crates/bixverse-atac/tests/fixtures/pbmc3k_chr22"
  )
)
atac_frags <- file.path(atac_fix, "fragments.tsv.gz")
atac_gtf <- file.path(atac_fix, "genes.gtf.gz")

if (!file.exists(atac_frags)) {
  exit_file("ATAC fixtures not found; set BIXVERSE_ATAC_FIXTURES")
}

atac_dir <- tempfile("atac_only")
dir.create(atac_dir)
atac <- load_atac_fragments(
  SingleCellsAtac(atac_dir),
  atac_frags,
  peaks = file.path(atac_fix, "peaks.bed"),
  mode = "fragment",
  barcodes = readLines(file.path(atac_fix, "cells.txt")),
  .verbose = FALSE
)
atac <- run_tfidf_sc(atac)
atac <- find_top_features_sc(atac, min_cutoff = "q5")
atac <- calculate_lsi_sc(
  atac,
  lsi_params = params_atac_lsi(n_comp = 10L, solver = "exact"),
  .verbose = FALSE
)

### plots on bixverse output ---------------------------------------------------

p <- lsi_depth_plot_sc(get_lsi_depth_cor(atac))
expect_true(
  checkmate::checkClass(p, "ggplot"),
  info = "lsi_depth_plot_sc: bixverse output"
)

profiles <- fragment_profiles_sc(atac, atac_frags, atac_gtf)
expect_true(
  checkmate::checkClass(tss_profile_plot_sc(profiles$tss), "ggplot"),
  info = "tss_profile_plot_sc: bixverse output"
)
expect_true(
  checkmate::checkClass(fragment_length_plot_sc(profiles$lengths), "ggplot"),
  info = "fragment_length_plot_sc: bixverse output"
)

### atac modality --------------------------------------------------------------

p <- embedding_plot_sc(
  object = atac,
  embedding = "lsi",
  colour_by = "lib_size",
  embd_modality = "atac"
)
expect_true(
  checkmate::checkClass(p, "ggplot"),
  info = "embedding_plot_sc: atac modality"
)

peak <- get_gene_names(atac)[1]
p <- feature_plot_sc(
  object = atac,
  features = peak,
  embedding = "lsi",
  expr_modality = "atac",
  embd_modality = "atac"
)
expect_true(
  checkmate::checkClass(p, "patchwork"),
  info = "feature_plot_sc: atac modality"
)

### gene activity on the atac embedding ----------------------------------------

ga <- gene_activity_sc(atac, atac_frags, atac_gtf, .verbose = FALSE)
gene <- get_gene_names(ga)[1]
p <- feature_plot_sc(
  object = atac,
  features = gene,
  embedding = "lsi",
  embd_modality = "atac",
  expr_sc = ga,
  scale_mode = "shared"
)
expect_true(
  checkmate::checkClass(p, "ggplot"),
  info = "feature_plot_sc: expression from expr_sc"
)
expect_equal(
  length(unique(p$data$cell_id)),
  length(get_cell_names(atac, TRUE)),
  info = "feature_plot_sc: expr_sc keeps every cell"
)

ga_sub <- set_cells_to_keep(ga, get_cell_names(ga, TRUE)[1:100])
expect_error(
  feature_plot_sc(
    object = atac,
    features = gene,
    embedding = "lsi",
    embd_modality = "atac",
    expr_sc = ga_sub
  ),
  pattern = "same cells",
  info = "feature_plot_sc: expr_sc with other cells"
)
