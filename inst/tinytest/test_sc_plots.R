# sc plotting -----------------------------------------------------------------

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
p <- plot_qc_violin(
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
p <- plot_qc_density(
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
p <- plot_joined_qc(
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
