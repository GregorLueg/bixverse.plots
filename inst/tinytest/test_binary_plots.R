# binary heatmap ---------------------------------------------------------------

## data ------------------------------------------------------------------------

set.seed(123L)
grp <- rep(c("g1", "g2"), each = 50L)
blk <- rep(c("g1", "g2"), each = 5L)
binary_mat <- xor(outer(grp, blk, "=="), matrix(runif(1000L) < 0.1, 100L))
dimnames(binary_mat) <- list(sprintf("cell_%i", 1:100), sprintf("tf_%i", 1:10))
sample_groups <- setNames(grp, rownames(binary_mat))
feature_groups <- setNames(blk, colnames(binary_mat))

## tests -----------------------------------------------------------------------

p <- plot_binary_heatmap(
  binary_mat,
  sample_groups = sample_groups,
  feature_groups = feature_groups
)

expect_inherits(
  current = p,
  class = "ggplot",
  info = "binary heatmap - matrix input returns a ggplot"
)

# one raster per sample group x feature group block
expect_equal(
  current = sum(purrr::map_lgl(p$layers, \(l) {
    inherits(l$geom, "GeomRasterAnn")
  })),
  target = 4L,
  info = "binary heatmap - one raster per block"
)

hm_data <- bixverse::extract_binary_heatmap_data(binary_mat, .verbose = FALSE)

expect_inherits(
  current = plot_binary_heatmap(hm_data),
  class = "ggplot",
  info = "binary heatmap - BinaryHeatmapData input returns a ggplot"
)

expect_error(
  current = plot_binary_heatmap(binary_mat * 1),
  info = "binary heatmap - numeric matrix is rejected"
)

p_annot <- plot_binary_heatmap(
  binary_mat,
  sample_groups = sample_groups,
  group_annotation = c(g1 = "sensitive", g2 = "resistant")
)

# the first rect layer is the annotation strip
annot_layer <- purrr::detect_index(
  p_annot$layers,
  \(l) inherits(l$geom, "GeomRect")
)

expect_equal(
  current = sort(ggplot2::get_layer_data(p_annot, i = annot_layer)$fill),
  target = sort(bx_colors("viridis", n = 2L)),
  info = "binary heatmap - annotation strip gets one rect per label"
)

expect_error(
  current = plot_binary_heatmap(
    binary_mat,
    sample_groups = sample_groups,
    group_annotation = c(g1 = "sensitive")
  ),
  info = "binary heatmap - annotation must cover every sample group"
)
