# mitch tests ------------------------------------------------------------------

## synthetic data --------------------------------------------------------------

synthetic_data <- data.table::data.table(
  pathway_names = c("A", "B", "C"),
  manova_fdr = c(1, 0.1, 0.05),
  s_dist = c(1.25, 1, 0.25),
  p.contrast_a = c(0.25, 0.01, 0.001),
  p.contrast_b = c(0.002, 0.05, 0.5),
  s.contrast_a = c(0.1, -0.1, 0.7),
  s.contrast_b = c(0.5, -0.25, 0.1)
)

synthetic_data_bad <- data.table::copy(synthetic_data)

synthetic_data_bad[, manova_fdr := NULL]

### expected results -----------------------------------------------------------

# manually calculated

expected_fdr_case_1 <- matrix(data = c(0.003, 0.5), nrow = 1)
expected_score_case_1 <- matrix(data = c(0.7, 0), nrow = 1)

expected_fdr_case_2 <- matrix(
  data = c(0.015, 0.075, 0.003, 0.5),
  nrow = 2,
  byrow = TRUE
)
expected_score_case_2 <- matrix(
  data = c(-0.1, -0.25, 0.7, 0),
  nrow = 2,
  byrow = TRUE
)

rownames(expected_fdr_case_1) <- rownames(expected_score_case_1) <- "C"
rownames(expected_fdr_case_2) <- rownames(expected_score_case_2) <- c("B", "C")
colnames(expected_fdr_case_1) <- colnames(expected_fdr_case_2) <- c(
  "contrast_a_fdr",
  "contrast_b_fdr"
)
colnames(expected_score_case_1) <- colnames(expected_score_case_2) <- c(
  "contrast_a_score",
  "contrast_b_score"
)

## tests -----------------------------------------------------------------------

### error ----------------------------------------------------------------------

expect_error(
  current = prepare_mitch_scores(synthetic_data_bad),
  info = "mitch helper throws error with bad input"
)

### test scenarios -------------------------------------------------------------

# with fdr = 0.05
results_case_1 <- prepare_mitch_scores(synthetic_data, fdr_threshold = 0.05)

expect_equal(
  current = results_case_1$fdr_corrections,
  target = expected_fdr_case_1,
  info = "mitch helper scenario 1 - right output fdr"
)

expect_equal(
  current = results_case_1$adj_scores,
  target = expected_score_case_1,
  info = "mitch helper scenario 1 - right output fdr"
)

# with fdr = 0.1 - should give more

results_case_2 <- prepare_mitch_scores(synthetic_data, fdr_threshold = 0.1)

expect_equal(
  current = results_case_2$fdr_corrections,
  target = expected_fdr_case_2,
  info = "mitch helper scenario 1 - right output fdr"
)

expect_equal(
  current = results_case_2$adj_scores,
  target = expected_score_case_2,
  info = "mitch helper scenario 1 - right output fdr"
)
