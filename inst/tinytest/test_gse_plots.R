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

oae_res <- bixverse::gse_hypergeometric_list(
  target_genes = target_genes_ls,
  gene_set_list = all_pathways,
  threshold = 0.99,
  minimum_overlap = 1L
)

expect_message(
  current = plot_gse_dotplot(res = oae_res),
  info = paste("list version - correct message")
)

p <- plot_gse_dotplot(res = oae_res, .verbose = FALSE)

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
