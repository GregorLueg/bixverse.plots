# blitzgsea plots --------------------------------------------------------------

## helpers ---------------------------------------------------------------------

#' Interpolate the blitzGSEA gamma parameters at a given gene set size
#'
#' @description
#' Mirrors `interp_log_linear_at()` on the Rust side: bracket on the anchor
#' grid, then interpolate linearly in log space. Note that the bracket index is
#' clamped to the interior, so values outside the grid are *extrapolated* along
#' the terminal segment rather than held flat. That is deliberate,
#' [stats::approx()] with `rule = 2` would clamp instead and quietly disagree
#' with the p-values the scoring returned.
#'
#' @param null_model `BlitzGseaNull` object from
#' [bixverse::blitzgsea_calibrate()].
#' @param size Numeric. The gene set size to evaluate at.
#'
#' @returns A named list with `shape_pos`, `scale_pos`, `shape_neg`,
#' `scale_neg` and `pos_ratio`, the last one clamped to `[0, 1]`.
#'
#' @keywords internal
blitz_tail_at <- function(null_model, size) {
  # checks
  assertBlitzGseaNull(null_model)
  checkmate::qassert(size, "N1")

  x <- null_model[["anchor_sizes"]]
  size <- max(size, 1)
  n <- length(x)

  # partition_point: number of knots <= size. Clamped to the interior so the
  # terminal segments extrapolate.
  j <- min(max(sum(x <= size), 1L), n - 1L)

  interp <- if (size <= 0 || x[j] <= 0 || x[j + 1L] <= 0) {
    \(y) y[j] + (y[j + 1L] - y[j]) * (size - x[j]) / (x[j + 1L] - x[j])
  } else {
    dx <- log(x[j + 1L]) - log(x[j])
    if (dx <= 0) {
      \(y) y[j]
    } else {
      \(y) y[j] + (y[j + 1L] - y[j]) * (log(size) - log(x[j])) / dx
    }
  }

  list(
    shape_pos = interp(null_model[["shape_pos"]]),
    scale_pos = interp(null_model[["scale_pos"]]),
    shape_neg = interp(null_model[["shape_neg"]]),
    scale_neg = interp(null_model[["scale_neg"]]),
    pos_ratio = min(max(interp(null_model[["pos_ratio"]]), 0), 1)
  )
}

#' Evaluate the blitzGSEA null density
#'
#' @description
#' The null is a two-sided mixture: the positive tail carries weight
#' `pos_ratio`, the negative tail the rest, and each is a gamma on the absolute
#' enrichment score. Same decomposition the p-value is read off.
#'
#' @param es Numeric vector. Enrichment scores to evaluate the density at.
#' @param tail List. Output of [bixverse.plots::blitz_tail_at()].
#'
#' @returns Numeric vector of densities, same length as `es`.
#'
#' @keywords internal
blitz_null_density <- function(es, tail) {
  # checks
  checkmate::assertNumeric(es, any.missing = FALSE)
  checkmate::assertList(tail, types = "numeric")

  # branch on the sign rather than ifelse(), which would evaluate both arms and
  # hand dgamma a negative value
  pos <- es > 0
  out <- numeric(length(es))
  out[pos] <- tail$pos_ratio *
    stats::dgamma(es[pos], shape = tail$shape_pos, scale = tail$scale_pos)
  out[!pos] <- (1 - tail$pos_ratio) *
    stats::dgamma(-es[!pos], shape = tail$shape_neg, scale = tail$scale_neg)

  return(out)
}

## plotting --------------------------------------------------------------------

### calibration ----------------------------------------------------------------

#' Plot the blitzGSEA calibration
#'
#' @description
#' Diagnostic for a `BlitzGseaNull`. Shows the smoothed gamma parameters against
#' anchor set size for both tails, plus the fraction of positive null scores.
#' A fit that sags at one end of the size range is obvious here and invisible in
#' the results table.
#'
#' The KS goodness-of-fit p-values are in the subtitle. Low values mean the
#' gamma tails do not describe the permutation scores well and the p-values are
#' optimistic. More permutations is the usual fix.
#'
#' @param null_model `BlitzGseaNull` object from
#' [bixverse::blitzgsea_calibrate()].
#' @param line_size Numeric. Line width. Defaults to `0.8`.
#' @param point_size Numeric. Point size for the anchor knots. Defaults to
#' `1.2`.
#'
#' @returns A `ggplot` object, facetted over the parameter type.
#'
#' @import ggplot2
#'
#' @export
#'
#' @references Lachmann, et al., Bioinformatics, 2022
plot_blitzgsea_null <- function(
  null_model,
  line_size = 0.8,
  point_size = 1.2
) {
  # globals scope check
  parameter <- tail <- value <- anchor_size <- NULL

  # checks
  assertBlitzGseaNull(null_model)
  checkmate::qassert(line_size, "N1")
  checkmate::qassert(point_size, "N1")

  anchor_sizes <- null_model[["anchor_sizes"]]

  plot_dt <- data.table::rbindlist(list(
    data.table::data.table(
      anchor_size = anchor_sizes,
      value = null_model[["shape_pos"]],
      parameter = "shape",
      tail = "positive"
    ),
    data.table::data.table(
      anchor_size = anchor_sizes,
      value = null_model[["shape_neg"]],
      parameter = "shape",
      tail = "negative"
    ),
    data.table::data.table(
      anchor_size = anchor_sizes,
      value = null_model[["scale_pos"]],
      parameter = "scale",
      tail = "positive"
    ),
    data.table::data.table(
      anchor_size = anchor_sizes,
      value = null_model[["scale_neg"]],
      parameter = "scale",
      tail = "negative"
    ),
    data.table::data.table(
      anchor_size = anchor_sizes,
      value = null_model[["pos_ratio"]],
      parameter = "positive fraction",
      tail = "both"
    )
  ))

  plot_dt[,
    parameter := factor(
      parameter,
      levels = c("shape", "scale", "positive fraction")
    )
  ]

  subtitle <- sprintf(
    "KS p-value: %.3g (positive tail), %.3g (negative tail)",
    null_model[["ks_pos"]],
    null_model[["ks_neg"]]
  )
  if (!is.null(null_model[["n_genes"]])) {
    subtitle <- sprintf(
      "%s | %i genes, centred: %s",
      subtitle,
      null_model[["n_genes"]],
      null_model[["centred"]]
    )
  }

  p <- ggplot(
    data = plot_dt,
    mapping = aes(x = anchor_size, y = value, colour = tail)
  ) +
    geom_line(linewidth = line_size) +
    geom_point(size = point_size) +
    facet_wrap(~parameter, scales = "free_y", ncol = 1L) +
    scale_x_log10() +
    scale_color_bx() +
    xlab("Anchor gene set size") +
    ylab("Smoothed value") +
    labs(
      title = "blitzGSEA null calibration",
      subtitle = subtitle,
      colour = "Tail"
    ) +
    theme_bx()

  return(p)
}

### enrichment score vs null ---------------------------------------------------

#' Plot observed enrichment scores against the blitzGSEA null
#'
#' @description
#' For each pathway of interest, draws the fitted null density at that
#' pathway's size and drops the observed enrichment score on top, with the tail
#' beyond it shaded. This is where the p-value comes from: blitzGSEA never
#' permutes per pathway, it reads the score off the gamma tail interpolated to
#' the set size.
#'
#' The raw permutation scores never cross the Rust boundary, so this is the
#' fitted density, not a histogram of null draws.
#'
#' @param null_model `BlitzGseaNull` object from
#' [bixverse::blitzgsea_calibrate()]. Has to be the null the results were
#' scored against.
#' @param res data.table. Output of [bixverse::calc_blitzgsea()]. Needs the
#' columns `c("pathway_name", "es", "size", "pvals", "fdr")`.
#' @param pathways_of_interest String vector. Names of the pathways to plot.
#' These need to be represented in `res$pathway_name`.
#' @param n_points Integer. Resolution of the density curve. Defaults to `512L`.
#' @param text_size Numeric. Size of the annotation text. Defaults to `3`.
#'
#' @returns A named list of `ggplot` objects, one per element of
#' `pathways_of_interest`.
#'
#' @import ggplot2
#'
#' @export
#'
#' @references Lachmann, et al., Bioinformatics, 2022
plot_blitzgsea_es_null <- function(
  null_model,
  res,
  pathways_of_interest,
  n_points = 512L,
  text_size = 3
) {
  # globals scope check
  pathway_name <- density <- es <- NULL

  # checks
  assertBlitzGseaNull(null_model)
  checkmate::assertDataTable(res)
  checkmate::assertNames(
    names(res),
    must.include = c("pathway_name", "es", "size", "pvals", "fdr")
  )
  checkmate::qassert(pathways_of_interest, "S+")
  checkmate::assertTRUE(all(
    pathways_of_interest %in% res[["pathway_name"]]
  ))
  checkmate::qassert(n_points, "I1[16,)")
  checkmate::qassert(text_size, "N1")

  plots <- purrr::map(pathways_of_interest, \(pathway) {
    row <- res[pathway_name == pathway][1L]
    tail <- blitz_tail_at(null_model, row[["size"]])
    observed <- row[["es"]]

    # far enough out that the observed score is never off the canvas
    limit <- max(
      abs(observed) * 1.2,
      stats::qgamma(0.999, shape = tail$shape_pos, scale = tail$scale_pos),
      stats::qgamma(0.999, shape = tail$shape_neg, scale = tail$scale_neg)
    )
    grid <- seq(-limit, limit, length.out = n_points)

    curve_dt <- data.table::data.table(
      es = grid,
      density = blitz_null_density(grid, tail)
    )
    tail_dt <- if (observed > 0) {
      curve_dt[es >= observed]
    } else {
      curve_dt[es <= observed]
    }

    label <- sprintf(
      "ES: %.3f\nNES: %.3f\np: %.3e\nFDR: %.3e\nsize: %i",
      observed,
      row[["nes"]],
      row[["pvals"]],
      row[["fdr"]],
      as.integer(row[["size"]])
    )

    ggplot(
      data = curve_dt,
      mapping = aes(x = es, y = density)
    ) +
      geom_area(
        data = tail_dt,
        fill = if (observed > 0) "#8b3a2b" else "#235070",
        alpha = 0.4
      ) +
      geom_line(linewidth = 0.7, colour = "grey20") +
      geom_vline(
        xintercept = observed,
        linetype = "dashed",
        colour = "grey20"
      ) +
      annotate(
        "text",
        x = -limit,
        y = max(curve_dt$density),
        label = label,
        hjust = 0,
        vjust = 1,
        size = text_size
      ) +
      xlab("Enrichment score") +
      ylab("Fitted null density") +
      labs(title = wrap_and_truncate(pathway, width = 60L)) +
      theme_bx()
  })

  names(plots) <- pathways_of_interest

  return(plots)
}
