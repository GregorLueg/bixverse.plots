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
