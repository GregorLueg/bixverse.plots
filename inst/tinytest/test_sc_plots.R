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

# multi-feature faceting
features_multi <- c("gene_001", "gene_050", "gene_100")
p <- feature_plot_sc(
  object = sc_object,
  features = features_multi,
  embedding = "umap"
)
expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "feature_plot (multi): correct class"
)
expect_equal(
  current = length(unique(ggplot2::ggplot_build(p)$data[[1]]$PANEL)),
  target = length(features_multi),
  info = "feature_plot (multi): one panel per feature"
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

## cleanup ---------------------------------------------------------------------

on.exit(
  {
    unlink(test_temp_dir, recursive = TRUE, force = TRUE)
    unlink(test_temp_dir_mm, recursive = TRUE, force = TRUE)
  },
  add = TRUE
)
