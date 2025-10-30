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
#' @param .verbose Boolean. Controls verbosity of the function.
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
#' @param resolution Numeric. The resolution parameter for the Louvain
#' clustering.
#' @param layout_func Layout function. Please see [igraph::add_layout_()]
#' for options. This one will be used to layout the graph.
#' @param ... Further parameters to forward to
#' [bixverse.plots::wrap_and_truncate()].
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
  resolution = 1.0,
  layout_func = igraph::layout_with_fr,
  ...
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
  communities <- igraph::cluster_louvain(g, resolution = resolution)
  igraph::V(g)$community <- igraph::membership(communities)

  # add layout coordinates
  layout <- layout_func(g)
  g$layout <- layout

  igraph::V(g)$label <- vapply(
    igraph::V(g)$name,
    wrap_and_truncate,
    character(1),
    ...
  )
  igraph::V(g)$neg_log10_fdr <- -log10(res[
    match(enriched_gs, gene_set_name),
    fdr
  ])

  return(g)
}

## ggraph ----------------------------------------------------------------------

#' igraph enrichment map to ggraph plot
#'
#' @description
#' Takes in the output from [bixverse.plots::enrichment_map_oae()] and generates
#' a ggraph object for subsequent saving, etc.
#'
#' @param g igraph. Output from [bixverse.plots::enrichment_map_oae()].
#' @param label_nodes String. Controls which nodes to label. Options:
#' - `"all"`: Label all nodes
#' - `"adaptive"`: Adaptive labelling based on community size (default)
#' - `NULL`: No labels
#' - Integer: Label top N nodes by size
#' @param labels_to_include Optional string. These are labels you want to
#' include no matter what.
#' @param adaptive_thresholds Named numeric. The names indicate the community
#' size and the values how many pathways per community to show. An example would
#' be `c(15 = 3, 5 = 2, 2 = 1, 1 = 0)`
#' @param font_size Numeric. Font size of the labels on top of the enrichment
#' map.
#' @param ... Other parameters you wish to forward to
#' [ggraph::geom_node_text()].
#'
#' @returns A ggplot2 object with the enrichment map
#'
#' @export
plot_enrichment_map_ggraph <- function(
  g,
  label_nodes = "adaptive",
  labels_to_include = NULL,
  adaptive_thresholds = c("15" = 3, "5" = 2, "2" = 1, "1" = 0),
  font_size = 4,
  ...
) {
  # checks
  checkmate::assertClass(g, "igraph")
  checkmate::assertTRUE(!is.null(igraph::V(g)$community))
  checkmate::assertTRUE(!is.null(igraph::V(g)$size))
  checkmate::assert(
    checkmate::check_string(label_nodes),
    checkmate::check_int(label_nodes, lower = 1),
    checkmate::check_null(label_nodes)
  )
  checkmate::assertNumeric(
    adaptive_thresholds,
    lower = 0,
    names = "named"
  )
  checkmate::qassert(font_size, "N1")
  checkmate::qassert(labels_to_include, c("0", "S+"))

  # determine which nodes to label
  node_data <- data.table::data.table(
    name = igraph::V(g)$name,
    label = igraph::V(g)$label,
    size = igraph::V(g)$size,
    community = igraph::V(g)$community
  )

  # add community sizes
  node_data[, comm_size := .N, by = community]

  if (is.null(label_nodes)) {
    # no labels
    node_data[, label := NA_character_]
  } else if (is.character(label_nodes) && label_nodes == "all") {
    # keep all labels as is
  } else if (is.character(label_nodes) && label_nodes == "adaptive") {
    # determine number of labels per community based on community size
    thresholds_sorted <- adaptive_thresholds[order(as.numeric(names(
      adaptive_thresholds
    )))]
    threshold_sizes <- as.numeric(names(thresholds_sorted))

    node_data[,
      n_labels := {
        cs <- comm_size[1]
        idx <- findInterval(cs, threshold_sizes, left.open = FALSE)
        if (idx > 0) thresholds_sorted[idx] else 0
      },
      by = community
    ]

    node_data[, rank := frank(-size, ties.method = "first"), by = community]
    node_data[rank > n_labels, label := NA_character_]
  } else if (is.numeric(label_nodes)) {
    # label by top X size
    keep <- node_data[order(-size)][1:min(label_nodes, .N)]$name
    node_data[!name %in% keep, label := NA_character_]
  }

  # override with labels_to_include
  if (!is.null(labels_to_include)) {
    node_data[name %in% labels_to_include, label := name]
  }

  igraph::V(g)$label <- node_data$label

  p <- ggraph::ggraph(graph = g, layout = g$layout) +
    ggraph::geom_edge_link(
      mapping = aes(width = weight),
      edge_colour = "grey70",
      alpha = 0.6
    ) +
    ggraph::geom_node_point(
      mapping = aes(size = size, fill = neg_log10_fdr),
      shape = 21,
      colour = "grey90",
      stroke = 0.5
    ) +
    ggraph::geom_node_text(
      mapping = aes(label = label),
      repel = TRUE,
      size = font_size,
      fontface = "bold",
      ...
    ) +
    ggraph::scale_edge_width(range = c(0.3, 2)) +
    ggplot2::scale_size(range = c(3, 8)) +
    ggplot2::scale_fill_distiller(
      palette = "Blues",
      direction = 1,
      name = "-log10(FDR)"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))

  p
}

## visnetwork ------------------------------------------------------------------

#' igraph enrichment map to VisNetwork interactive network
#'
#' @description
#' Takes in the output from [bixverse.plots::enrichment_map_oae()] and generates
#' an interactive VisNetwork widget.
#'
#' @param g igraph. Output from [bixverse.plots::enrichment_map_oae()].
#'
#' @returns The interactive visnetwork
#'
#' @export
#'
#' @importFrom magrittr `%>%`
plot_enrichment_map_visnetwork <- function(g) {
  . <- NULL
  # checks
  checkmate::assertClass(g, "igraph")
  checkmate::assertTRUE(!is.null(igraph::V(g)$community))
  checkmate::assertTRUE(!is.null(igraph::V(g)$size))
  checkmate::assertTRUE(!is.null(igraph::V(g)$neg_log10_fdr))

  layout_coords <- g$layout

  # map -log10(FDR) to Blues palette
  fdr_values <- igraph::V(g)$neg_log10_fdr
  colour_palette <- scales::col_numeric(
    palette = "Blues",
    domain = range(fdr_values),
    reverse = FALSE
  )
  node_colours <- colour_palette(fdr_values)

  nodes <- igraph::as_data_frame(g, what = "vertices") %>%
    data.table::as.data.table() %>%
    .[, `:=`(
      id = name,
      label = label,
      value = size,
      color = node_colours,
      title = paste0(
        name,
        "\nSize: ",
        round(size, 2),
        "\n-log10(FDR): ",
        round(neg_log10_fdr, 2)
      )
    )] %>%
    .[, .(id, label, value, color, title)]

  edges <- igraph::as_data_frame(g, what = "edges") %>%
    data.table::as.data.table() %>%
    .[, `:=`(
      title = paste0("Similarity: ", round(weight, 3))
    )] %>%
    .[, .(from, to, value = weight, title)]

  visnetwork <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visIgraphLayout(
      layout = 'layout.norm',
      layoutMatrix = layout_coords
    ) %>%
    visNetwork::visNodes(
      borderWidth = 1,
      color = list(border = "grey90")
    ) %>%
    visNetwork::visEdges(color = list(color = "grey70")) %>%
    visNetwork::visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = TRUE
    ) %>%
    visNetwork::visInteraction(hover = TRUE)

  visnetwork
}
