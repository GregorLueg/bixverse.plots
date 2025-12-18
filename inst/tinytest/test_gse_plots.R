# gse plotting -----------------------------------------------------------------

## data ------------------------------------------------------------------------

set.seed(123L)
target_genes_ls <- purrr::map(
  1:5,
  ~ {
    sample(letters, sample(4:10, 1))
  }
)
names(target_genes_ls) <- sprintf("exp_%i", 1:5)

# fully random pathways
pathway_genes_ls <- purrr::map(
  1:10,
  ~ {
    sample(letters, sample(4:10, 1))
  }
)
pathway_gene_ls_not_random <- purrr::map(target_genes_ls, \(genes) {
  new_genes <- sample(letters, 3)
  unique(c(new_genes, genes[-c(1:2)]))
})
all_pathways <- do.call(c, list(pathway_genes_ls, pathway_gene_ls_not_random))

names(all_pathways) <- sprintf("gs_%02i", 1:15)

result <- bixverse::gse_hypergeometric_list(
  target_genes_list = target_genes_ls,
  gene_set_list = all_pathways,
  threshold = 0.99,
  minimum_overlap = 1L
)

result <- bixverse::gse_hypergeometric(
  target_genes = target_genes_ls[[2]],
  gene_set_list = all_pathways,
  threshold = 0.99,
  minimum_overlap = 1L
)

## dot plot function -----------------------------------------------------------

### single case ----------------------------------------------------------------

oae_res <- bixverse::gse_hypergeometric(
  target_genes = target_genes_ls[[2]],
  gene_set_list = all_pathways,
  threshold = 0.99,
  minimum_overlap = 1L
)

# assertion tests
expect_error(
  current = plot_gse_dotplot(res = oae_res, size_range = 3),
  info = "dotplot - wrong size scale"
)
expect_error(
  current = plot_gse_dotplot(res = as.data.frame(oae_res), size_range = 3),
  info = "dotplot - wrong input class"
)
expect_error(
  current = plot_gse_dotplot(res = oae_res, viridis_option = "Z"),
  info = "dotplot - wrong viridis option"
)

# correct plot generation
p <- plot_gse_dotplot(res = oae_res)

expect_true(current = "ggplot" %in% class(p), info = "dotplot - single version")

### list case ------------------------------------------------------------------

oae_res_ls <- bixverse::gse_hypergeometric_list(
  target_genes = target_genes_ls,
  gene_set_list = all_pathways,
  threshold = 0.99,
  minimum_overlap = 1L
)

expect_message(
  current = plot_gse_dotplot(res = oae_res_ls),
  info = paste("list version - correct message")
)

p <- plot_gse_dotplot(res = oae_res_ls, .verbose = FALSE)

expect_true(
  class(p) == "list",
  info = paste("list version - a list is returned")
)
expect_true(
  length(p) == 5,
  info = paste("list version - expected length")
)
expect_true(
  current = all(purrr::map_lgl(
    p,
    ~ {
      "ggplot" %in% class(.x)
    }
  )),
  info = paste("list version - every element is a ggplot object")
)

## enrichment map version ------------------------------------------------------

### igraph part - OAE ----------------------------------------------------------

enrichr_map_pathways <- list(
  "pathway_a" = letters[1:3],
  "pathway_b" = letters[1:4],
  "pathway_c" = letters[2:7],
  "pathway_d" = letters[3:7],
  "pathway_e" = letters[20:26],
  "pathway_f" = letters[18:23],
  "pathway_g" = letters[22:24]
)
enrichr_target_genes <- c(letters[2:5], letters[16], letters[21:25])
enrichr_result <- bixverse::gse_hypergeometric(
  target_genes = enrichr_target_genes,
  gene_set_list = enrichr_map_pathways,
  threshold = 0.99,
  minimum_overlap = 1L
)
enrichment_map_igraph <- enrichment_map_oae(
  res = enrichr_result,
  threshold = 1.0,
  pathways = enrichr_map_pathways
)
expect_true(
  checkmate::checkClass(enrichment_map_igraph, "igraph"),
  info = "enrichment map is an igraph"
)
expect_true(
  igraph::vcount(enrichment_map_igraph) > 0,
  info = "graph has vertices"
)
expect_true(
  igraph::ecount(enrichment_map_igraph) > 0,
  info = "graph has edges"
)
expect_true(
  all(
    c("size", "community", "label", "neg_log10_fdr", "color_value") %in%
      igraph::vertex_attr_names(enrichment_map_igraph)
  ),
  info = "graph has required vertex attributes"
)
expect_equal(
  enrichment_map_igraph$color_type,
  "fdr",
  info = "OAE graph has correct color_type"
)

### igraph part - GSEA ---------------------------------------------------------

gsea_map_pathways <- list(
  "pathway_a" = letters[1:3],
  "pathway_b" = letters[1:4],
  "pathway_c" = letters[2:7],
  "pathway_d" = letters[3:7],
  "pathway_e" = letters[20:26],
  "pathway_f" = letters[18:23],
  "pathway_g" = letters[22:24]
)
gsea_result <- data.table::data.table(
  geneset_name = c("pathway_c", "pathway_d", "pathway_e", "pathway_f"),
  nes = c(2.1, 1.8, -1.9, -2.3),
  fdr = c(0.01, 0.02, 0.015, 0.005)
)
enrichment_map_igraph_gsea <- enrichment_map_gsea(
  res = gsea_result,
  threshold = 1.0,
  pathways = gsea_map_pathways
)
expect_true(
  checkmate::checkClass(enrichment_map_igraph_gsea, "igraph"),
  info = "GSEA enrichment map is an igraph"
)
expect_true(
  igraph::vcount(enrichment_map_igraph_gsea) > 0,
  info = "GSEA graph has vertices"
)
expect_true(
  igraph::ecount(enrichment_map_igraph_gsea) > 0,
  info = "GSEA graph has edges"
)
expect_true(
  all(
    c("size", "community", "label", "nes", "color_value") %in%
      igraph::vertex_attr_names(enrichment_map_igraph_gsea)
  ),
  info = "GSEA graph has required vertex attributes"
)
expect_equal(
  enrichment_map_igraph_gsea$color_type,
  "nes",
  info = "GSEA graph has correct color_type"
)

### ggraph - OAE ---------------------------------------------------------------

enrichment_map_ggraph <- plot_enrichment_map_ggraph(
  enrichment_map_igraph,
  label_nodes = "all"
)
expect_true(
  checkmate::checkClass(enrichment_map_ggraph, c("ggraph", "ggplot")),
  info = "correct plot classes returned"
)
expect_equal(
  length(enrichment_map_ggraph$layers),
  3L,
  info = "plot has expected number of layers"
)
expect_true(
  "GeomEdgePath" %in%
    sapply(enrichment_map_ggraph$layers, function(x) class(x$geom)[1]),
  info = "plot contains edge layer"
)
expect_true(
  "GeomPoint" %in%
    sapply(enrichment_map_ggraph$layers, function(x) class(x$geom)[1]),
  info = "plot contains node layer"
)

# Test adaptive labelling
enrichment_map_adaptive <- plot_enrichment_map_ggraph(
  enrichment_map_igraph,
  label_nodes = "adaptive"
)
n_labels <- sum(!is.na(enrichment_map_adaptive$data$label))
expect_true(
  n_labels < igraph::vcount(enrichment_map_igraph),
  info = "adaptive labelling reduces number of labels"
)

# Test NULL labelling
enrichment_map_no_labels <- plot_enrichment_map_ggraph(
  enrichment_map_igraph,
  label_nodes = NULL
)
expect_true(
  all(is.na(enrichment_map_no_labels$data$label)),
  info = "NULL removes all labels"
)

# Test labels_to_include overrides adaptive labelling
specific_labels <- igraph::V(enrichment_map_igraph)$name[1:2]
enrichment_map_forced <- plot_enrichment_map_ggraph(
  enrichment_map_igraph,
  label_nodes = NULL,
  labels_to_include = specific_labels
)
expect_true(
  all(specific_labels %in% na.omit(enrichment_map_forced$data$label)),
  info = "labels_to_include forces specific labels to show"
)
expect_equal(
  sum(!is.na(enrichment_map_forced$data$label)),
  length(specific_labels),
  info = "only forced labels are shown when label_nodes is NULL"
)

### ggraph - GSEA --------------------------------------------------------------

enrichment_map_ggraph_gsea <- plot_enrichment_map_ggraph(
  enrichment_map_igraph_gsea,
  label_nodes = "all"
)
expect_true(
  checkmate::checkClass(enrichment_map_ggraph_gsea, c("ggraph", "ggplot")),
  info = "GSEA plot has correct classes"
)
expect_equal(
  length(enrichment_map_ggraph_gsea$layers),
  3L,
  info = "GSEA plot has expected number of layers"
)

### visnetwork - OAE -----------------------------------------------------------

enrichment_map_vis <- plot_enrichment_map_visnetwork(enrichment_map_igraph)
expect_true(
  checkmate::checkClass(enrichment_map_vis, c("visNetwork", "htmlwidget")),
  info = "visNetwork returns correct classes"
)
expect_true(
  nrow(enrichment_map_vis$x$nodes) == igraph::vcount(enrichment_map_igraph),
  info = "visNetwork has correct number of nodes"
)
expect_true(
  nrow(enrichment_map_vis$x$edges) == igraph::ecount(enrichment_map_igraph),
  info = "visNetwork has correct number of edges"
)

### visnetwork - GSEA ----------------------------------------------------------

enrichment_map_vis_gsea <- plot_enrichment_map_visnetwork(
  enrichment_map_igraph_gsea
)
expect_true(
  checkmate::checkClass(enrichment_map_vis_gsea, c("visNetwork", "htmlwidget")),
  info = "GSEA visNetwork returns correct classes"
)
expect_true(
  nrow(enrichment_map_vis_gsea$x$nodes) ==
    igraph::vcount(enrichment_map_igraph_gsea),
  info = "GSEA visNetwork has correct number of nodes"
)
expect_true(
  nrow(enrichment_map_vis_gsea$x$edges) ==
    igraph::ecount(enrichment_map_igraph_gsea),
  info = "GSEA visNetwork has correct number of edges"
)
