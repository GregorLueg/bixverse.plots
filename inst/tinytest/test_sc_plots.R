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

## test ------------------------------------------------------------------------

## Violin plot
p <- violin_plot_sc(
  df = df,
  grouping_column = "donor_id",
  variable = "nnz",
  var_name = "# Features",
  show_outlier = F
)

expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "correct plot classes returned"
)

## Density plot
p <- density_plot_sc(
  df = df,
  grouping_column = "donor_id",
  variable = "nnz",
  var_name = "# Features"
)

expect_true(
  checkmate::checkClass(p, c("ggplot")),
  info = "correct plot classes returned"
)

## Joint plot
p <- joint_plot_sc(
  df = df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = TRUE
)

expect_true(
  checkmate::checkClass(
    p,
    c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
  ),
  info = "correct plot classes returned"
)

## Single cell object plots

## Create single cell test object
test_temp_dir <- file.path(
  tempdir(),
  "gs_activitiy"
)

dir.create(test_temp_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot("Test directory does not exist" = dir.exists(test_temp_dir))

## testing parameters ----------------------------------------------------------

# thresholds
min_lib_size <- 300L
min_genes_exp <- 45L
min_cells_exp <- 500L
# hvg
hvg_to_keep <- 30L
# pca
no_pcs <- 10L

## synthetic test data ---------------------------------------------------------

single_cell_test_data <- generate_single_cell_test_data()

genes_pass <- which(
  Matrix::colSums(single_cell_test_data$counts != 0) >= min_cells_exp
)

cells_pass <- which(
  (Matrix::rowSums(single_cell_test_data$counts[, genes_pass]) >=
    min_lib_size) &
    (Matrix::rowSums(single_cell_test_data$counts[, genes_pass] != 0) >=
      min_genes_exp)
)

## underlying class ------------------------------------------------------------

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

sc_object <- find_hvg_sc(
  object = sc_object,
  hvg_no = hvg_to_keep,
  .verbose = FALSE
)

sc_object <- calculate_pca_sc(sc_object, no_pcs = no_pcs, .verbose = FALSE)
sc_object <- find_neighbours_sc(
  object = sc_object,
  neighbours_params = params_sc_neighbours(
    full_snn = FALSE,
    pruning = 0,
    knn = list(knn_method = "kmknn")
  )
)

sc_object <- find_clusters_sc(
  sc_object,
  res = 0.5,
  name = "leiden_clusters",
  cluster_algorithm = "leiden"
)

# Dimensionality reduction
sc_object <- umap_sc(
  sc_object,
  knn_method = "kmknn",
  umap_params = manifoldsR::params_umap(init = "pca")
)

## Embedding plot
p <- embedding_plot_sc(
  object = sc_object,
  embedding = "umap",
  colour_by = "cell_grp",
  label_by = "cell_grp",
  point_alpha = 0.5
)

expect_true(
  checkmate::checkClass(
    p,
    c("ggplot")
  ),
  info = "correct plot classes returned"
)

## Feature plot
p <- feature_plot_sc(
  object = sc_object,
  features = "gene_001",
  feature_labels = c(gene_001 = "ens_001"),
  embedding = "umap",
  label_by = "cell_grp"
)

expect_true(
  checkmate::checkClass(
    p,
    c("ggplot")
  ),
  info = "correct plot classes returned"
)


## dot plot
dot_plot_sc(
  object = sc_object,
  features = c("gene_001", "gene_002", "gene_097", "gene_100"),
  grouping_variable = "cell_grp",
  scale_exp = TRUE,
  cluster_groups = T
)

expect_true(
  checkmate::checkClass(
    p,
    c("ggplot")
  ),
  info = "correct plot classes returned"
)

## violin plot
p <- stacked_violin_plot_sc(
  sc_object,
  features = c("gene_001", "gene_100"),
  grouping_variable = "cell_grp"
)

expect_true(
  checkmate::checkClass(
    p,
    c("ggplot")
  ),
  info = "correct plot classes returned"
)
