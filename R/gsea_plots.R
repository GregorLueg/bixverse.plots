# (f)gsea plots ----------------------------------------------------------------

## plotting --------------------------------------------------------------------

#' Plot GSEA enrichment results
#'
#' @description
#' Helper function to create the classical GSEA plots for a set of pathways
#' of interest. You can also provide the underlying GSEA results to add the
#' FDR and NES information to the plot.
#'
#' @param stats Named numeric vector. The gene level statistic.
#' @param pathways List. A named list with each element containing the genes for
#' this pathway.
#' @param pathways_of_interest String vector. Names of the pathways to plot.
#' These strings need to be represented in the names of pathways.
#' @param gsea_results Optional data.table with the bixverse GSEA results. If
#' provided, the FDR and NES for the given pathway of interest will be also
#' added to the plot.
#' @param gsea_param Numeric. Defaults to `1`.
#' @param tick_size Numeric. The tick size. Defaults ot `0.2`.
#' @param text_size Numeric. The text size. Defaults ot `8`. Only relevant when
#' `gsea_results` is provided.
#'
#' @import ggplot2
#'
#' @export
plot_gsea_enrichment <- function(
  stats,
  pathways,
  pathways_of_interest,
  gsea_results = NULL,
  gsea_param = 1,
  tick_size = 0.2,
  text_size = 5
) {
  # checks
  checkmate::assertNumeric(
    stats,
    min.len = 3L,
    finite = TRUE,
    names = "named"
  )
  checkmate::assertList(pathways, types = "character", names = "named")
  checkmate::qassert(pathways_of_interest, "S+")
  checkmate::assertTRUE(all(pathways_of_interest %in% names(pathways)))
  checkmate::qassert(gsea_param, "N1")
  checkmate::qassert(tick_size, "N1")
  if (!is.null(gsea_results)) {
    checkmate::assertDataTable(gsea_results)
    checkmate::assertNames(
      names(gsea_results),
      must.include = c("pathway_name", "nes", "fdr")
    )
    checkmate::assertTRUE(all(
      pathways_of_interest %in% gsea_results[["pathway_name"]]
    ))
  }

  # generate the plotting data
  plot_data <- get_gsea_enrichment_data(
    stats = stats,
    pathways = pathways,
    pathways_of_interest = pathways_of_interest,
    gsea_results = gsea_results,
    gsea_param = gsea_param
  )

  # iterate through the plots
  plots <- purrr::imap(plot_data, \(pd, gs_name) {
    p <- with(
      pd,
      ggplot(data = curve_dt) +
        geom_line(aes(x = rank, y = ES), color = "green") +
        geom_segment(
          data = ticks_dt,
          mapping = aes(
            x = rank,
            y = -key_points["spread_es"] / 16,
            xend = rank,
            yend = key_points["spread_es"] / 16
          ),
          linewidth = tick_size
        ) +
        geom_hline(
          yintercept = key_points["pos_es"],
          colour = "red",
          linetype = "dashed"
        ) +
        geom_hline(
          yintercept = key_points["neg_es"],
          colour = "red",
          linetype = "dashed"
        ) +
        geom_hline(yintercept = 0, colour = "black") +
        theme(
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey92")
        ) +
        labs(x = "Rank", y = "Enrichment Score") +
        ggtitle(gs_name)
    )

    if (pd$additional_label) {
      x_label_coord <- ifelse(
        sign(pd$key_points[["nes"]]) == 1,
        ceiling(nrow(pd$stats_dt) * 0.7),
        ceiling(nrow(pd$stats_dt) * 0.1)
      )
      y_label_coord <- ifelse(
        sign(pd$key_points[["nes"]]) == 1,
        pd$key_points[["pos_es"]] * 0.7,
        pd$key_points[["neg_es"]] * 0.7
      )
      p <- p +
        geom_text(
          x = x_label_coord,
          y = y_label_coord,
          label = sprintf(
            "NES: %.3f\nFDR: %.3e",
            pd$key_points["nes"],
            pd$key_points["fdr"]
          ),
          hjust = 0,
          size = text_size
        )
    }

    p
  })

  return(plots)
}

## helpers ---------------------------------------------------------------------

#' Helper function to get the plot data for GSEA plots
#'
#' @param stats Named numeric vector. The gene level statistic.
#' @param pathways List. A named list with each element containing the genes for
#' this pathway.
#' @param pathways_of_interest String vector. Names of the pathways to plot.
#' These strings need to be represented in the names of pathways.
#' @param gsea_results Optional data.table with the bixverse GSEA results. If
#' provided, the FDR and NES for the given pathway of interest will be also
#' added to the plot.
#' @param gsea_param Numeric. Defaults to `1`
#'
#' @return A list of `gsea_par_plot_data`
#'
#' @importFrom zeallot `%<-%`
#' @import data.table
get_gsea_enrichment_data <- function(
  stats,
  pathways,
  pathways_of_interest,
  gsea_results = NULL,
  gsea_param = 1
) {
  # checks
  checkmate::assertNumeric(
    stats,
    min.len = 3L,
    finite = TRUE,
    names = "named"
  )
  checkmate::assertList(pathways, types = "character", names = "named")
  checkmate::qassert(pathways_of_interest, "S+")
  checkmate::assertTRUE(any(pathways_of_interest %in% names(pathways)))
  checkmate::qassert(gsea_param, "N1")
  if (!is.null(gsea_results)) {
    checkmate::assertDataTable(gsea_results)
    checkmate::assertNames(
      names(gsea_results),
      must.include = c("pathway_name", "nes", "fdr")
    )
    checkmate::assertTRUE(all(
      pathways_of_interest %in% gsea_results[["pathway_name"]]
    ))
  }

  # prepare the data
  c(stats, pathways_ls, pathway_sizes) %<-%
    bixverse:::prep_stats_pathways(
      stats = stats,
      pathways = pathways[pathways_of_interest],
      min_size = 3L,
      max_size = 100000L
    )

  plot_data <- purrr::imap(pathways_ls, \(pathway, gs_name) {
    c(es, leading_edge, top, bottom) %<-%
      bixverse::rs_calc_gsea_stats(
        stats = stats,
        gs_idx = pathway,
        gsea_param = gsea_param,
        return_leading_edge = FALSE,
        return_all_extremes = TRUE
      )

    n <- length(stats)
    # little trick to concatenate the vectors easier
    xs <- as.vector(rbind(pathway - 1, pathway))
    ys <- as.vector(rbind(bottom, top))
    curve_dt <- data.table::data.table(
      rank = c(0, xs, n + 1),
      ES = c(0, ys, 0)
    )
    ticks_dt <- data.table::data.table(
      rank = pathway,
      stat = stats[pathway]
    )
    stats_dt <- data.table::data.table(
      rank = seq_along(stats),
      stat = stats
    )
    key_points <- unlist(list(
      pos_es = max(top),
      neg_es = min(bottom),
      spread_es = max(top) - min(bottom)
    ))
    if (!is.null(gsea_results)) {
      nes_fdr <- gsea_results[
        pathway_name == gs_name,
        c(nes = nes, fdr = fdr)
      ]
      key_points <- c(key_points, nes_fdr)
    }
    additional_label <- !is.null(gsea_results)

    res <- list(
      curve_dt = curve_dt,
      ticks_dt = ticks_dt,
      stats_dt = stats_dt,
      key_points = key_points,
      additional_label = additional_label
    )

    class(res) <- "gsea_par_plot_data"

    res
  })

  return(plot_data)
}
