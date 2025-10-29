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

library(magrittr)

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

devtools::load_all()

g <- enrichment_map_oae(
  res = enrichr_result,
  threshold = 1.0,
  pathways = enrichr_map_pathways
)

plot(x)


igraph::V(g)$community
igraph::V(g)$size

plot_enrichment_map_ggraph <- function(g) {
  ggraph::ggraph(g, layout = g$layout) +
    ggraph::geom_edge_link(
      aes(width = weight),
      edge_colour = "grey70",
      alpha = 0.6
    ) +
    ggraph::geom_node_point(
      aes(size = size, fill = factor(community)),
      shape = 21,
      colour = "black",
      stroke = 0.5
    ) +
    ggraph::geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3
    ) +
    ggraph::scale_edge_width(range = c(0.3, 2)) +
    ggplot2::scale_size(range = c(3, 8)) +
    ggplot2::scale_fill_viridis_d(guide = "none") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
}

plot_enrichment_map_visnetwork <- function(g) {
  layout_coords <- g$layout
  community_colours <- viridis::viridis(length(unique(igraph::V(g)$community)))

  nodes <- igraph::as_data_frame(g, what = "vertices") %>%
    data.table::as.data.table() %>%
    .[, `:=`(
      id = name,
      label = name,
      value = size,
      color = community_colours[community],
      title = paste0(name, "\n(Size: ", round(size, 2), ")")
    )] %>%
    .[, .(id, label, value, color, title)]

  edges <- igraph::as_data_frame(g, what = "edges") %>%
    data.table::as.data.table() %>%
    .[, `:=`(
      title = paste0("Similarity: ", round(weight, 3))
    )] %>%
    .[, .(from, to, value = weight, title)]

  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visIgraphLayout(
      layout = 'layout.norm',
      layoutMatrix = layout_coords
    ) %>%
    visNetwork::visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = TRUE
    )
}


plot_enrichment_map_ggraph(x)

plot_enrichment_map_visnetwork(x)

g <- x


layout_coords <- g$layout
community_colours <- viridis::viridis(length(unique(igraph::V(g)$community)))

nodes <- igraph::as_data_frame(g, what = "vertices") %>%
  data.table::as.data.table() %>%
  .[, `:=`(
    id = name,
    label = name,
    value = size,
    x = layout_coords[, 1],
    y = layout_coords[, 2],
    color = community_colours[community],
    title = paste0(name, "\n(Size: ", round(size, 2), ")")
  )] %>%
  .[, .(id, label, value, x, y, color, title)]

edges <- igraph::as_data_frame(g, what = "edges") %>%
  data.table::as.data.table() %>%
  .[, `:=`(
    title = paste0("Similarity: ", round(weight, 3))
  )] %>%
  .[, .(from, to, value = weight, title)]

visNetwork::visNetwork(nodes, edges) %>%
  visNetwork::visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = TRUE
  )
