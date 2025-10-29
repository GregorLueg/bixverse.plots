# dot plots --------------------------------------------------------------------

## plotting --------------------------------------------------------------------

#' Generate GSE dotplots
#'
#' @description
#' This function can take in the output of [bixverse::gse_hypergeometric()] or
#' [bixverse::gse_hypergeometric_list()] and generates in the former case a
#' single plot and in the latter case a list of plots per target set.
#'
#' @param res data.table with the enrichment results. Needs to have the columns
#' `c("hits", "target_set_lengths", "gene_set_name", "fdr")`.
#' @param size_range Numerical vector of size 2. Defines the size range for the
#' dots in the plot.
#' @param viridis_option String. The option to forward to
#' [ggplot2::scale_fill_viridis_c()].
#' @param direction `1` or `-1`. The direction in the colour palette.
#'
#' @return If the output of [bixverse::gse_hypergeometric_list()] was provided,
#' a list of dotplots per target gene set. Otherwise, a single GSE OAE dot plot.
#'
#' @export
plot_gse_dotplot <- function(
  res,
  size_range = c(2, 5),
  viridis_option = "D",
  direction = -1,
  .verbose = TRUE
) {
  # checks
  checkmate::assertDataTable(res)
  checkmate::assertNames(
    names(res),
    must.include = c("hits", "target_set_lengths", "gene_set_name", "fdr")
  )
  checkmate::qassert(size_range, "N2")
  checkmate::assertChoice(viridis_option, LETTERS[1:8])
  checkmate::assertChoice(direction, c(-1, 1))
  checkmate::qassert(.verbose, "B1")

  # split if several target sets were tested
  res <- if ('target_set_name' %in% names(res)) {
    if (.verbose) {
      message(
        paste(
          "Several target sets were found.",
          "Splitting and returning a list of plots."
        )
      )
    }
    split(res, res$target_set_name)
  } else {
    res
  }

  # generate the plots
  p <- switch(
    class(res)[[1]],
    "list" = {
      purrr::map(
        res,
        helper_gse_dot_plot,
        size_range = size_range,
        viridis_option = viridis_option,
        direction = direction
      )
    },
    "data.table" = helper_gse_dot_plot(
      res,
      size_range = size_range,
      viridis_option = viridis_option,
      direction = direction
    )
  )

  p
}

## helpers ---------------------------------------------------------------------

#' Helper to generate a GSE dot plot
#'
#' @param res data.table with the enrichment results. Needs to have the columns
#' `c("hits", "target_set_lengths", "gene_set_name", "fdr")`.
#' @param size_range Numerical vector of size 2. Defines the size range for the
#' dots in the plot.
#' @param viridis_option String. The option to forward to
#' [ggplot2::scale_fill_viridis_c()].
#' @param direction `1` or `-1`. The direction in the colour palette.
#'
#' @return The GSE dot plot.
#'
#' @import ggplot2
helper_gse_dot_plot <- function(
  res,
  size_range = c(2, 5),
  viridis_option = "D",
  direction = -1
) {
  # checks
  checkmate::assertDataTable(res)
  checkmate::assertNames(
    names(res),
    must.include = c("hits", "target_set_lengths", "gene_set_name", "fdr")
  )
  checkmate::qassert(size_range, "N2")
  checkmate::assertChoice(viridis_option, LETTERS[1:8])
  checkmate::assertChoice(direction, c(-1, 1))

  # data processing
  res <- data.table::copy(res)[, gene_ratio := hits / target_set_lengths]
  data.table::setorder(res, gene_ratio)
  order <- res[, gene_set_name]
  res[, gene_set_name := factor(x = gene_set_name, levels = order)]
  max_val <- res[, max(gene_ratio)] + 0.05

  # plot
  p <- ggplot(
    data = res,
    mapping = aes(x = gene_ratio, y = gene_set_name)
  ) +
    geom_point(
      mapping = aes(fill = fdr, size = gene_set_lengths),
      shape = 21
    ) +
    xlim(0, max_val) +
    xlab("Gene Ratio") +
    ylab("Geneset name") +
    theme_bw() +
    scale_size_continuous(range = size_range) +
    labs(size = "GS size", fill = "FDR") +
    scale_fill_viridis_c(direction = direction, option = viridis_option)

  return(p)
}

# enrichment map ---------------------------------------------------------------

## igraph part -----------------------------------------------------------------

#' Generate enrichment map igraph
#'
#' @description
#' Helper function to generate an enrichment map based on overenrichment
#' results. Similar enriched gene sets are clustered together via their
#' Jaccard similarity (alternatively overlap coefficient) and the function
#' returns an igraph object for subsequent visualisations.
#'
#' @param res data.table with the enrichment results. Needs to have the columns
#' `c("gene_set_lengths", "gene_set_name", "fdr")`.
#' @param threshold Numeric. The FDR threshold you wish to filter for.
#' @param pathways Named list. The original pathway list used for the
#' calculation of the overenrichment analysis.
#' @param overlap_coefficient Boolean. Shall the overlap coefficient be used
#' instead of the Jaccard similarity.
#' @param min_sim Numeric. Minimum similarity between two gene sets to be
#' connected.
#' @param layout_func Layout function. Please see [igraph::add_layout_()]
#' for options. This one will be used to layout the graph
#'
#' @return `igraph` object representing the enrichment map.
#'
#' @export
enrichment_map_oae <- function(
  res,
  threshold,
  pathways,
  overlap_coefficient = FALSE,
  min_sim = 0.2,
  layout_func = igraph::layout_with_fr
) {
  # checks
  checkmate::assertDataTable(res)
  checkmate::assertNames(
    names(res),
    must.include = c("gene_set_lengths", "gene_set_name", "fdr")
  )
  checkmate::assertList(pathways, types = "character", names = "named")
  checkmate::qassert(overlap_coefficient, "B1")
  checkmate::qassert(min_sim, "N[0, 1]")
  checkmate::assertFunction(layout_func)

  # calculate similarities
  enriched_gs <- res[["gene_set_name"]]
  edges <- data.table::setDT(bixverse::rs_set_similarity_list(
    list = pathways[enriched_gs],
    overlap_coefficient = overlap_coefficient
  ))[sim >= min_sim]

  g <- igraph::graph_from_data_frame(
    d = edges[, .(from, to, weight = sim)],
    directed = FALSE,
    vertices = enriched_gs
  )

  # node size
  igraph::V(g)$size <- log10(lengths(pathways[enriched_gs]))

  # community detection
  communities <- igraph::cluster_louvain(g)
  igraph::V(g)$community <- igraph::membership(communities)

  # add layout coordinates
  layout <- layout_func(g)
  g$layout <- layout
  return(g)

  return(g)
}

## ggraph ----------------------------------------------------------------------

#' igraph enrichment map to ggraph plot
#'
#' @description
#' Takes in the output from [bixverse.plots::enrichment_map_oae()] and generates
#' a ggraph object for subsequent saving,etc.
#'
plot_enrichment_map_ggraph <- function(g) {
  # checks
  checkmate::assertClass(g, "igraph")
  checkmate::assertTRUE(!is.null(g$community))
  checkmate::assertTRUE(!is.null(g$size))

  p <- ggraph::ggraph(graph = g, layout = g$layout) +
    ggraph::geom_edge_link(
      mapping = aes(width = weight),
      edge_colour = "grey70",
      alpha = 0.6
    ) +
    ggraph::geom_node_point(
      mapping = aes(size = size, fill = factor(community)),
      shape = 21,
      colour = "black",
      stroke = 0.5
    ) +
    ggraph::geom_node_text(
      mapping = aes(label = name),
      repel = TRUE,
      size = 3
    ) +
    ggraph::scale_edge_width(range = c(0.3, 2)) +
    ggplot2::scale_size(range = c(3, 8)) +
    ggplot2::scale_fill_viridis_d(guide = "none") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))

  p
}
