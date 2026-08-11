# sc plotting ------------------------------------------------------------------

library(bixverse)

## data ------------------------------------------------------------------------

n_cells <- 500
df <- data.table::data.table(
  donor_id = sample(paste0("D", 1:5), n_cells, replace = TRUE),
  nnz = rnbinom(n_cells, mu = 2000, size = 5),
  lib_size = rnbinom(n_cells, mu = 3000, size = 10)
)
# Simulate a low-quality donor
df[donor_id == "D5", nnz := rnbinom(.N, mu = 200, size = 5)]

## data.table interface --------------------------------------------------------

### violin plot ----------------------------------------------------------------

p <- violin_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "nnz",
  var_name = "# Features",
  show_outlier = FALSE
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "violin_plot.dt (no outlier): correct class"
)

# outlier branch: recomputes per group via per_cell_qc_outlier
p <- violin_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "nnz",
  direction = "below",
  show_outlier = TRUE
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "violin_plot.dt (outlier): correct class"
)

# raster path
p <- suppressMessages(violin_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "nnz",
  show_outlier = TRUE,
  raster = TRUE
))
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "violin_plot.dt (raster): correct class"
)

### density plot ---------------------------------------------------------------

p <- density_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "nnz",
  var_name = "# Features"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "density_plot.dt: correct class"
)

# log_scale + label offset
p <- density_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "lib_size",
  log_scale = TRUE,
  adjust_position_label = 100
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "density_plot.dt (log + label adj): correct class"
)

### joint plot -----------------------------------------------------------------

p <- joint_plot_sc(
  x = df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = TRUE
)
expect_true(
  checkmate::checkClass(
    p,
    c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
  ),
  info = "joint_plot.dt (log): correct class"
)

p <- joint_plot_sc(
  x = df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE
)
expect_true(
  checkmate::checkClass(
    p,
    c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
  ),
  info = "joint_plot.dt (linear): correct class"
)

## single cell test object -----------------------------------------------------

set.seed(42L)

test_temp_dir <- file.path(tempdir(), "sc_plotting")
dir.create(test_temp_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot("Test directory does not exist" = dir.exists(test_temp_dir))

# thresholds
min_lib_size <- 300L
min_genes_exp <- 45L
min_cells_exp <- 500L
hvg_to_keep <- 30L
no_pcs <- 10L

single_cell_test_data <- generate_single_cell_test_data()

sc_object <- SingleCells(dir_data = test_temp_dir)

sc_object <- load_r_data(
  object = sc_object,
  counts = single_cell_test_data$counts,
  obs = single_cell_test_data$obs,
  var = single_cell_test_data$var,
  sc_qc_param = params_sc_min_quality(
    min_unique_genes = min_genes_exp,
    min_lib_size = min_lib_size,
    min_cells = min_cells_exp
  ),
  streaming = 0L,
  .verbose = FALSE
)

sc_object <- find_hvg_sc(sc_object, hvg_no = hvg_to_keep, .verbose = FALSE)
sc_object <- calculate_pca_sc(sc_object, no_pcs = no_pcs, .verbose = FALSE)
sc_object <- find_neighbours_sc(
  object = sc_object,
  neighbours_params = params_sc_neighbours(
    full_snn = FALSE,
    pruning = 0,
    knn = list(knn_method = "kmknn")
  ),
  .verbose = FALSE
)
sc_object <- find_clusters_sc(
  sc_object,
  res = 0.5,
  name = "leiden_clusters",
  cluster_algorithm = "leiden"
)
sc_object <- umap_sc(
  sc_object,
  knn_method = "kmknn",
  umap_params = manifoldsR::params_umap(init = "pca"),
  .verbose = FALSE
)

### embedding plot -------------------------------------------------------------

p <- embedding_plot_sc(
  object = sc_object,
  embedding = "umap",
  colour_by = "cell_grp",
  label_by = "cell_grp",
  point_alpha = 0.5
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "embedding_plot (discrete + label): correct class"
)

# continuous colour scale branch
p <- embedding_plot_sc(
  object = sc_object,
  embedding = "umap",
  colour_by = "lib_size",
  discrete = FALSE
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "embedding_plot (continuous): correct class"
)

# raster path
p <- suppressMessages(embedding_plot_sc(
  object = sc_object,
  embedding = "umap",
  colour_by = "cell_grp",
  raster = TRUE
))
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "embedding_plot (raster): correct class"
)

### feature plot ---------------------------------------------------------------

p <- feature_plot_sc(
  object = sc_object,
  features = "gene_001",
  feature_labels = c(gene_001 = "ens_001"),
  embedding = "umap",
  label_by = "cell_grp"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_plot (single): correct class"
)

# multi-feature, free scales (default)
features_multi <- c("gene_001", "gene_050", "gene_100")
p <- feature_plot_sc(
  object = sc_object,
  features = features_multi,
  embedding = "umap"
)
expect_true(
  checkmate::checkClass(p, c("patchwork")),
  info = "feature_plot (multi, free): correct class"
)
expect_equal(
  current = length(p$patches$plots) + 1L,
  target = length(features_multi),
  info = "feature_plot (multi, free): one plot per feature"
)

# multi-feature, shared scale
p <- feature_plot_sc(
  object = sc_object,
  features = features_multi,
  embedding = "umap",
  scale_mode = "shared"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_plot (multi, shared): correct class"
)
expect_equal(
  current = length(unique(ggplot2::ggplot_build(p)$data[[1]]$PANEL)),
  target = length(features_multi),
  info = "feature_plot (multi, shared): one panel per feature"
)

# spectral palette
p <- feature_plot_sc(
  object = sc_object,
  features = features_multi,
  embedding = "umap",
  palette = "spectral"
)
expect_true(
  checkmate::checkClass(p, c("patchwork")),
  info = "feature_plot (spectral): correct class"
)

# highlight branch (sparse-gene path in .plot_embedding)
p <- feature_plot_sc(
  object = sc_object,
  features = "gene_001",
  embedding = "umap",
  highlight_features = TRUE,
  highlight_quantile = 0.5
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_plot (highlight): correct class"
)

# z-scored + clipped
p <- feature_plot_sc(
  object = sc_object,
  features = "gene_001",
  embedding = "umap",
  scale = TRUE,
  clip = 3
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_plot (scaled): correct class"
)

### dot plot -------------------------------------------------------------------

# original test never captured the return value; now bound
p <- dot_plot_sc(
  object = sc_object,
  features = c("gene_001", "gene_002", "gene_097", "gene_100"),
  grouping_variable = "cell_grp",
  scale_exp = TRUE,
  cluster_groups = TRUE
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "dot_plot: correct class"
)

# no clustering, no scaling
p <- dot_plot_sc(
  object = sc_object,
  features = c("gene_001", "gene_002", "gene_097", "gene_100"),
  grouping_variable = "cell_grp",
  scale_exp = FALSE,
  cluster_groups = FALSE
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "dot_plot (no clustering): correct class"
)

# feature_labels + feature_grouping: faceted branch and the chained
# label->group assertion
feature_labels <- c(
  gene_001 = "A1",
  gene_002 = "A2",
  gene_097 = "B1",
  gene_100 = "B2"
)
feature_grouping <- c(
  A1 = "group_A",
  A2 = "group_A",
  B1 = "group_B",
  B2 = "group_B"
)
p <- dot_plot_sc(
  object = sc_object,
  features = names(feature_labels),
  grouping_variable = "cell_grp",
  feature_labels = feature_labels,
  feature_grouping = feature_grouping,
  cluster_groups = TRUE
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "dot_plot (feature_grouping + labels): correct class"
)

### stacked violin -------------------------------------------------------------

p <- stacked_violin_plot_sc(
  sc_object,
  features = c("gene_001", "gene_100"),
  grouping_variable = "cell_grp"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "stacked_violin: correct class"
)

p <- stacked_violin_plot_sc(
  sc_object,
  features = c("gene_001", "gene_100"),
  feature_labels = c(gene_001 = "Gene A", gene_100 = "Gene B"),
  grouping_variable = "cell_grp",
  scale = TRUE,
  clip = 3
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "stacked_violin (labels + scaled): correct class"
)

### feature scatter ------------------------------------------------------------

p <- feature_scatter_plot_sc(
  object = sc_object,
  feature_1 = "gene_001",
  feature_2 = "gene_002",
  geom = "density"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_scatter (density): correct class"
)

p <- feature_scatter_plot_sc(
  object = sc_object,
  feature_1 = "gene_001",
  feature_2 = "gene_002",
  geom = "hex",
  bins = 30
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_scatter (hex): correct class"
)

p <- suppressMessages(feature_scatter_plot_sc(
  object = sc_object,
  feature_1 = "gene_001",
  feature_2 = "gene_002",
  geom = "density",
  raster = TRUE
))
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_scatter (raster): correct class"
)

## CellQc fixture --------------------------------------------------------------

qc_df <- sc_object[[c("cell_id", "cell_grp", "lib_size", "nnz")]]

qc_metrics <- list(
  log10_lib_size = log10(qc_df$lib_size),
  log10_nnz = log10(qc_df$nnz)
)
qc_directions <- c(
  log10_lib_size = "twosided",
  log10_nnz = "twosided"
)

qc <- run_cell_qc(
  metrics = qc_metrics,
  cells_to_keep = get_cells_to_keep(sc_object),
  directions = qc_directions,
  threshold = 3,
  groups = qc_df$cell_grp
)

### violin_plot_sc.CellQc ------------------------------------------------------

plots <- violin_plot_sc(qc)
expect_true(
  checkmate::checkList(plots, types = "ggplot"),
  info = "violin_plot.CellQc: returns a list of ggplots"
)
expect_equal(
  current = names(plots),
  target = names(qc_metrics),
  info = "violin_plot.CellQc: one plot per metric"
)

plots <- suppressMessages(violin_plot_sc(qc, raster = TRUE))
expect_true(
  checkmate::checkList(plots, types = "ggplot"),
  info = "violin_plot.CellQc (raster): list of ggplots"
)

### density_plot_sc.CellQc -----------------------------------------------------

plots <- density_plot_sc(qc)
expect_true(
  checkmate::checkList(plots, types = "ggplot"),
  info = "density_plot.CellQc: returns a list of ggplots"
)
expect_equal(
  current = names(plots),
  target = names(qc_metrics),
  info = "density_plot.CellQc: one plot per metric"
)

# ungrouped qc -> per_group_stats is NULL -> explicit stop()
qc_ungrouped <- run_cell_qc(
  metrics = qc_metrics,
  cells_to_keep = get_cells_to_keep(sc_object),
  directions = qc_directions,
  threshold = 3
)
expect_error(
  current = density_plot_sc(qc_ungrouped),
  info = "density_plot.CellQc: errors on ungrouped qc"
)

### joint_plot_sc.CellQc -------------------------------------------------------

p <- joint_plot_sc(qc)
expect_true(
  checkmate::checkClass(
    p,
    c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
  ),
  info = "joint_plot.CellQc: correct class"
)

## multi-modal -----------------------------------------------------------------

test_temp_dir_mm <- file.path(tempdir(), "sc_plotting_mm")
dir.create(test_temp_dir_mm, recursive = TRUE, showWarnings = FALSE)
stopifnot(
  "Multi-modal test directory does not exist" = dir.exists(test_temp_dir_mm)
)

rna <- generate_single_cell_test_data()
adt <- generate_single_cell_test_data_adt()

sc_mm <- SingleCellsMultiModal(dir_data = test_temp_dir_mm)

sc_mm <- load_r_data(
  object = sc_mm,
  counts = rna$counts,
  obs = rna$obs,
  var = rna$var,
  sc_qc_param = params_sc_min_quality(
    min_unique_genes = min_genes_exp,
    min_lib_size = min_lib_size,
    min_cells = min_cells_exp
  ),
  streaming = 0L,
  .verbose = FALSE
)

sc_mm <- add_adt_counts_sc(sc_mm, adt_counts = adt$counts, method = "clr")

# RNA: hvg, pca, knn, umap
sc_mm <- find_hvg_sc(sc_mm, hvg_no = hvg_to_keep, .verbose = FALSE)
sc_mm <- calculate_pca_sc(sc_mm, no_pcs = no_pcs, .verbose = FALSE)
sc_mm <- find_neighbours_sc(
  sc_mm,
  neighbours_params = params_sc_neighbours(knn = list(knn_method = "kmknn")),
  .verbose = FALSE
)
sc_mm <- umap_sc(sc_mm, knn_method = "kmknn", .verbose = FALSE)

# ADT: pca, knn, tsne (vignette confirms tsne_sc accepts modality = "adt")
sc_mm <- calculate_pca_adt_sc(sc_mm, no_pcs = 10L)
sc_mm <- find_neighbours_sc(
  sc_mm,
  modality = "adt",
  neighbours_params = params_sc_neighbours(knn = list(knn_method = "kmknn")),
  .verbose = FALSE
)
sc_mm <- tsne_sc(sc_mm, modality = "adt", .verbose = FALSE)

### embedding plot on the ADT modality -----------------------------------------

p <- embedding_plot_sc(
  object = sc_mm,
  embedding = "tsne",
  colour_by = "cell_grp",
  embd_modality = "adt"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "embedding_plot (embd_modality=adt): correct class"
)

### feature plot, cross-modality -----------------------------------------------

# ADT expression on the ADT embedding
p <- feature_plot_sc(
  object = sc_mm,
  features = "protein_01",
  embedding = "tsne",
  expr_modality = "adt",
  embd_modality = "adt"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_plot (adt on adt embedding): correct class"
)

# RNA expression painted onto the ADT embedding
p <- feature_plot_sc(
  object = sc_mm,
  features = "gene_001",
  embedding = "tsne",
  expr_modality = "rna",
  embd_modality = "adt"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_plot (rna on adt embedding): correct class"
)

### feature scatter, mixed modalities ------------------------------------------

p <- feature_scatter_plot_sc(
  object = sc_mm,
  feature_1 = "gene_001_rna",
  feature_2 = "protein_01_adt",
  geom = "density"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_scatter (rna vs adt, density): correct class"
)

p <- feature_scatter_plot_sc(
  object = sc_mm,
  feature_1 = "gene_001_rna",
  feature_2 = "protein_01_adt",
  geom = "hex",
  bins = 20
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_scatter (rna vs adt, hex): correct class"
)

## palette selection -----------------------------------------------------------

### helpers --------------------------------------------------------------------

# ggplot hands back #RRGGBBAA where alpha is baked in, bx_colors() only #RRGGBB
norm_col <- \(x) toupper(substr(x, 1L, 7L))

# colours a discrete bx scale ends up drawing for `n` levels. The fixed-length
# palettes return more colours than asked for when `n` is below their length,
# and discrete_scale() then takes the first `n`
expected_discrete <- \(palette, n) {
  sort(norm_col(bx_colors(palette = palette, n = n)[seq_len(n)]))
}

drawn_discrete <- \(p, aes_col = "colour", layer = 1L) {
  sort(unique(norm_col(ggplot2::ggplot_build(p)$data[[layer]][[aes_col]])))
}

# both ends of the continuous scale actually attached to the plot
scale_ends <- \(p, aes_name = "colour") {
  norm_col(p$scales$get_scales(aes_name)$palette(c(0, 1)))
}

expected_ends <- \(palette, n = 20) {
  pal <- norm_col(bx_colors(palette = palette, n = n))
  pal[c(1L, length(pal))]
}

n_grp <- length(unique(qc_df$cell_grp))
n_donor <- length(unique(df$donor_id))
dot_features <- c("gene_001", "gene_002", "gene_097", "gene_100")

### bx_colors ramp -------------------------------------------------------------

expect_equal(
  current = length(bx_colors("spectral", n = 30)),
  target = 30L,
  info = "bx_colors: fixed-length palette is ramped up to n"
)
expect_equal(
  current = length(bx_colors("main", n = 8)),
  target = 8L,
  info = "bx_colors: palettes that already respect n are unaffected"
)

# every palette must survive a discrete scale with more levels than it has
# colours, which is what the ramp exists for
many_levels <- data.table::data.table(
  x = runif(300),
  y = runif(300),
  g = factor(sample(paste0("c", 1:30), 300, replace = TRUE))
)
for (pal in c("main", "sequential", "diverging", "viridis", "spectral")) {
  p <- ggplot2::ggplot(many_levels, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_point() +
    scale_color_bx(palette = pal)
  expect_equal(
    current = length(drawn_discrete(p)),
    target = 30L,
    info = sprintf("scale_color_bx (%s): 30 levels get 30 colours", pal)
  )
}

### embedding plot -------------------------------------------------------------

p <- embedding_plot_sc(sc_object, "umap", colour_by = "cell_grp")
expect_equal(
  current = drawn_discrete(p),
  target = expected_discrete("main", n_grp),
  info = "embedding_plot (discrete): defaults to the main palette"
)

p <- embedding_plot_sc(
  sc_object,
  "umap",
  colour_by = "cell_grp",
  palette = "spectral"
)
expect_equal(
  current = drawn_discrete(p),
  target = expected_discrete("spectral", n_grp),
  info = "embedding_plot (discrete): honours palette"
)

p <- embedding_plot_sc(sc_object, "umap", colour_by = "lib_size")
expect_equal(
  current = scale_ends(p),
  target = expected_ends("sequential"),
  info = "embedding_plot (continuous): defaults to the sequential palette"
)

p <- embedding_plot_sc(
  sc_object,
  "umap",
  colour_by = "lib_size",
  palette = "viridis"
)
expect_equal(
  current = scale_ends(p),
  target = expected_ends("viridis"),
  info = "embedding_plot (continuous): honours palette"
)

expect_error(
  current = embedding_plot_sc(
    sc_object,
    "umap",
    colour_by = "cell_grp",
    palette = "not_a_palette"
  ),
  info = "embedding_plot: rejects an unknown palette"
)

### dot plot -------------------------------------------------------------------

p <- dot_plot_sc(sc_object, dot_features, "cell_grp")
expect_equal(
  current = scale_ends(p),
  target = expected_ends("sequential"),
  info = "dot_plot: defaults to the sequential palette"
)

p <- dot_plot_sc(sc_object, dot_features, "cell_grp", palette = "diverging")
expect_equal(
  current = scale_ends(p),
  target = expected_ends("diverging"),
  info = "dot_plot: honours palette"
)

expect_error(
  current = dot_plot_sc(sc_object, dot_features, "cell_grp", palette = "main"),
  info = "dot_plot: rejects a discrete-only palette"
)

### stacked violin -------------------------------------------------------------

vln_features <- c("gene_001", "gene_100")

p <- stacked_violin_plot_sc(sc_object, vln_features, "cell_grp")
expect_equal(
  current = drawn_discrete(p, aes_col = "fill"),
  target = expected_discrete("main", n_grp),
  info = "stacked_violin: defaults to the main palette"
)

p <- stacked_violin_plot_sc(
  sc_object,
  vln_features,
  "cell_grp",
  palette = "spectral"
)
expect_equal(
  current = drawn_discrete(p, aes_col = "fill"),
  target = expected_discrete("spectral", n_grp),
  info = "stacked_violin: honours palette"
)

### feature scatter ------------------------------------------------------------

p <- feature_scatter_plot_sc(sc_object, "gene_001", "gene_002")
expect_equal(
  current = scale_ends(p, "fill"),
  target = expected_ends("viridis"),
  info = "feature_scatter (density): defaults to the viridis palette"
)

p <- feature_scatter_plot_sc(
  sc_object,
  "gene_001",
  "gene_002",
  palette = "sequential"
)
expect_equal(
  current = scale_ends(p, "fill"),
  target = expected_ends("sequential"),
  info = "feature_scatter (density): honours palette"
)

p <- feature_scatter_plot_sc(
  sc_object,
  "gene_001",
  "gene_002",
  geom = "hex",
  bins = 30,
  palette = "spectral"
)
expect_equal(
  current = scale_ends(p, "fill"),
  target = expected_ends("spectral"),
  info = "feature_scatter (hex): honours palette"
)

# the raster branch maps colour, not fill. It used to get a fill scale and so
# no colour scale at all
p <- suppressMessages(feature_scatter_plot_sc(
  sc_object,
  "gene_001",
  "gene_002",
  raster = TRUE,
  palette = "spectral"
))
expect_equal(
  current = scale_ends(p, "colour"),
  target = expected_ends("spectral"),
  info = "feature_scatter (raster): colour scale is set and honours palette"
)

### qc plots -------------------------------------------------------------------

p <- violin_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "nnz",
  show_outlier = FALSE,
  palette = "diverging"
)
expect_equal(
  current = drawn_discrete(p, layer = 2L),
  target = expected_discrete("diverging", n_donor),
  info = "violin_plot.dt (no outlier): honours palette"
)

# the outlier jitter is coloured by outlier status, which is semantic and must
# stay clear of the palette
p <- violin_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "nnz",
  show_outlier = TRUE,
  palette = "spectral"
)
expect_false(
  current = any(
    drawn_discrete(p, layer = 2L) %in% expected_discrete("spectral", n_donor)
  ),
  info = "violin_plot.dt (outlier): outlier colours are not palette-driven"
)

p <- density_plot_sc(
  x = df,
  grouping_column = "donor_id",
  variable = "nnz",
  palette = "spectral"
)
expect_equal(
  current = drawn_discrete(p, aes_col = "fill"),
  target = expected_discrete("spectral", n_donor),
  info = "density_plot.dt: honours palette"
)

p <- joint_plot_sc(
  x = df,
  library_size = "lib_size",
  nb_features = "nnz",
  palette = "spectral"
)
expect_true(
  checkmate::checkClass(
    p,
    c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
  ),
  info = "joint_plot.dt (spectral): correct class"
)
expect_error(
  current = joint_plot_sc(
    x = df,
    library_size = "lib_size",
    nb_features = "nnz",
    palette = "main"
  ),
  info = "joint_plot.dt: rejects a discrete-only palette"
)

### CellQc methods -------------------------------------------------------------

plots <- violin_plot_sc(qc, show_outlier = FALSE, palette = "spectral")
expect_equal(
  current = drawn_discrete(plots[[1]], layer = 2L),
  target = expected_discrete("spectral", n_grp),
  info = "violin_plot.CellQc: honours palette"
)

plots <- density_plot_sc(qc, palette = "diverging")
expect_equal(
  current = drawn_discrete(plots[[1]], aes_col = "fill"),
  target = expected_discrete("diverging", n_grp),
  info = "density_plot.CellQc: honours palette"
)

p <- joint_plot_sc(qc, palette = "viridis")
expect_true(
  checkmate::checkClass(
    p,
    c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
  ),
  info = "joint_plot.CellQc (viridis): correct class"
)

## cleanup ---------------------------------------------------------------------

on.exit(
  {
    unlink(test_temp_dir, recursive = TRUE, force = TRUE)
    unlink(test_temp_dir_mm, recursive = TRUE, force = TRUE)
  },
  add = TRUE
)
