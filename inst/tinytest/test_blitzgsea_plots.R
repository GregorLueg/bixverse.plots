# blitzgsea plotting -----------------------------------------------------------

## test data -------------------------------------------------------------------

set.seed(123L)

stat_size <- 2000L

stats <- setNames(
  sort(rnorm(stat_size), decreasing = TRUE),
  paste0("gene", 1:stat_size)
)

pathway_list <- list(
  pathway_pos = sample(names(stats)[1:300], 30),
  pathway_neg = sample(names(stats)[1701:2000], 25),
  random_p1 = sample(names(stats), 40),
  random_p2 = sample(names(stats), 20)
)

blitz_params <- bixverse::params_blitzgsea(
  min_size = 15L,
  permutations = 1000L
)

null_model <- bixverse::blitzgsea_calibrate(
  stats = stats,
  blitz_params = blitz_params
)

blitz_res <- bixverse::calc_blitzgsea(
  stats = stats,
  pathways = pathway_list,
  blitz_params = blitz_params,
  null_model = null_model
)

## interpolation ---------------------------------------------------------------

# interpolating at a knot has to return that knot's value, otherwise the drawn
# density is not the one the p-value came off
knot_errors <- purrr::map_dbl(
  seq_along(null_model$anchor_sizes),
  \(i) {
    tail <- bixverse.plots:::blitz_tail_at(
      null_model,
      null_model$anchor_sizes[i]
    )
    max(abs(c(
      tail$shape_pos - null_model$shape_pos[i],
      tail$scale_pos - null_model$scale_pos[i],
      tail$shape_neg - null_model$shape_neg[i],
      tail$scale_neg - null_model$scale_neg[i],
      tail$pos_ratio - null_model$pos_ratio[i]
    )))
  }
)

expect_true(
  max(knot_errors) < 1e-12,
  info = "blitz_tail_at reproduces the anchor knots exactly"
)

expect_true(
  {
    tail <- bixverse.plots:::blitz_tail_at(null_model, 42)
    tail$pos_ratio >= 0 && tail$pos_ratio <= 1
  },
  info = "blitz_tail_at clamps pos_ratio to [0, 1]"
)

## null density ----------------------------------------------------------------

# the drawn density has to integrate to the reported one-sided tail probability
tail_probs <- purrr::map_dbl(seq_len(nrow(blitz_res)), \(i) {
  row <- blitz_res[i]
  tail <- bixverse.plots:::blitz_tail_at(null_model, row$size)
  limit <- max(
    abs(row$es) * 3,
    stats::qgamma(1 - 1e-8, shape = tail$shape_pos, scale = tail$scale_pos)
  )
  integral <- if (row$es > 0) {
    stats::integrate(
      \(x) bixverse.plots:::blitz_null_density(x, tail),
      row$es,
      limit,
      subdivisions = 2000L
    )$value
  } else {
    stats::integrate(
      \(x) bixverse.plots:::blitz_null_density(x, tail),
      -limit,
      row$es,
      subdivisions = 2000L
    )$value
  }
  abs(2 * integral - row$pvals) / row$pvals
})

expect_true(
  max(tail_probs) < 1e-4,
  info = "null density integrates to the reported p-value"
)

expect_true(
  all(
    bixverse.plots:::blitz_null_density(
      c(-2, -1, 0, 1, 2),
      bixverse.plots:::blitz_tail_at(null_model, 30)
    ) >=
      0
  ),
  info = "null density is non-negative on both sides of zero"
)

## calibration plot ------------------------------------------------------------

expect_error(
  current = plot_blitzgsea_null(unclass(null_model)),
  info = "calibration plot - rejects a plain list"
)

expect_error(
  current = plot_blitzgsea_null(null_model, line_size = "a"),
  info = "calibration plot - rejects a non-numeric line size"
)

p <- plot_blitzgsea_null(null_model)

expect_true(
  "ggplot" %in% class(p),
  info = "calibration plot - returns a ggplot"
)

expect_true(
  nrow(p$data) == 5L * length(null_model$anchor_sizes),
  info = "calibration plot - one row per anchor and parameter"
)

## enrichment score vs null ----------------------------------------------------

expect_error(
  current = plot_blitzgsea_es_null(
    null_model,
    blitz_res,
    "does_not_exist"
  ),
  info = "es null plot - rejects an unknown pathway"
)

expect_error(
  current = plot_blitzgsea_es_null(
    null_model,
    as.data.frame(blitz_res),
    blitz_res$pathway_name[1]
  ),
  info = "es null plot - rejects a data.frame"
)

pathways_of_interest <- blitz_res$pathway_name[1:2]

plots <- plot_blitzgsea_es_null(
  null_model,
  blitz_res,
  pathways_of_interest
)

expect_true(
  class(plots) == "list",
  info = "es null plot - returns a list"
)

expect_equal(
  names(plots),
  pathways_of_interest,
  info = "es null plot - list is named after the pathways"
)

expect_true(
  all(purrr::map_lgl(plots, \(x) "ggplot" %in% class(x))),
  info = "es null plot - every element is a ggplot"
)

expect_true(
  all(purrr::map_lgl(plots, \(x) {
    "GeomVline" %in% sapply(x$layers, \(l) class(l$geom)[1])
  })),
  info = "es null plot - observed score is marked with a vline"
)
