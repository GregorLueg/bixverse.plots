# plotting functions for mitch -------------------------------------------------

## utils -----------------------------------------------------------------------

#' Generate adjusted scores for plotting mitch results
#'
#' @description
#' Function that takes results from [bixverse::calc_mitch()], extracts the
#' individual scores and p-values for each contrast, runs an FDR correction
#' on top of the p-values and sets scores above the provided FDR threshold to
#' 0.
#'
#' @param res data.table. Output of [bixverse::calc_mitch()].
#' @param fdr_threshold Numeric. The FDR threshold you want to apply.
#'
#' @returns A list with the following two elements
#' \itemize{
#'  \item fdr_corrections - FDR-corrected scores as a matrix of the individual
#'  contrast p-values.
#'  \item adj_scores - The extract scores per contrast with scores above the
#'  individual FDR threshold set to 0.
#' }
#'
#' @export
prepare_mitch_scores <- function(res, fdr_threshold = 0.05) {
  # checks
  checkmate::assertDataTable(res)
  checkmate::assertNames(
    names(res),
    must.include = c("pathway_names", "manova_fdr", "s_dist")
  )
  checkmate::qassert(fdr_threshold, "N1[0, 1]")

  # function body
  setorder(res, -s_dist)

  # significant pathways
  significant_pathways <- res[manova_fdr <= fdr_threshold, pathway_names]

  # select the columns
  p_cols <- colnames(res) %like% "p\\."
  s_cols <- colnames(res) %like% "s\\."

  # calculate the fdr on top of the individual p-values
  fdrs <- res[, ..p_cols] %>%
    .[, lapply(.SD, p.adjust, method = "BH")] %>%
    as.matrix() %>%
    `rownames<-`(res$pathway_names) %>%
    {
      original_col_names <- colnames(.)
      colnames(.) <- paste(
        gsub("p\\.", "", original_col_names),
        "fdr",
        sep = "_"
      )
      .
    }

  scores <- res[, ..s_cols] %>%
    as.matrix() %>%
    `rownames<-`(res$pathway_names) %>%
    {
      original_col_names <- colnames(.)
      colnames(.) <- paste(
        gsub("s\\.", "", original_col_names),
        "score",
        sep = "_"
      )
      .
    }

  scores[fdrs > fdr_threshold] <- 0

  list(
    fdr_corrections = fdrs[significant_pathways, , drop = FALSE],
    adj_scores = scores[significant_pathways, , drop = FALSE]
  )
}
