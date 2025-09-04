# gsea plotting ----------------------------------------------------------------

## test data -------------------------------------------------------------------

library(magrittr)

set.seed(123)

stat_size <- 1000

stats <- setNames(
  sort(rnorm(stat_size), decreasing = TRUE),
  paste0("gene", 1:stat_size)
)

random_sizes <- c(10, 15, 8)
pathway_random <- purrr::map(
  random_sizes,
  ~ {
    sample(names(stats), .x)
  }
)
pathway_pos <- sample(names(stats)[1:150], 15)
pathway_neg <- sample(names(stats)[851:1000], 7)
gene_universe <- names(stats)

pathway_list <- list(
  pathway_pos = pathway_pos,
  pathway_neg = pathway_neg,
  random_p1 = pathway_random[[1]],
  random_p2 = pathway_random[[2]],
  random_p3 = pathway_random[[3]]
)

## bixverse fgsea --------------------------------------------------------------

internal_gsea_simple_res <- bixverse::calc_fgsea(
  stats = stats,
  pathways = pathway_list
) %>%
  data.table::setorder(pathway_name)

## gsea plots ------------------------------------------------------------------

plots_v1 <- plot_gsea_enrichment(
  stats = stats,
  pathways = pathway_list,
  pathways_of_interest = c("pathway_pos", "pathway_neg")
)

expect_true(
  class(plots_v1) == "list",
  info = paste("GSEA plot - correct list type")
)

expect_true(
  "ggplot" %in% class(plots_v1[[1]]),
  info = paste("GSEA plot - generates correctly a ggplot (1)")
)

expect_true(
  "ggplot" %in% class(plots_v1[[2]]),
  info = paste("GSEA plot - generates correctly a ggplot (2)")
)

plots_v2 <- plot_gsea_enrichment(
  stats = stats,
  pathways = pathway_list,
  pathways_of_interest = c("pathway_pos", "pathway_neg"),
  gsea_results = internal_gsea_simple_res
)

plots_v2$pathway_pos

expect_true(
  inherits(plots_v2[[1]]$layers[[6]]$geom, "GeomText"),
  info = paste("GSEA plot - generates correctly a ggplot with a GeomText")
)

expect_true(
  inherits(plots_v2[[2]]$layers[[6]]$geom, "GeomText"),
  info = paste("GSEA plot - generates correctly a ggplot with a GeomText")
)
