# plot params ------------------------------------------------------------------

spec_plots <- param_spec(
  name = "plots",
  title = "Wrapper function for standard plot parameters",
  checker = "Plot",
  label = "plot params",
  hint = "width, height need to be numericals and res an integer.",
  fields = list(
    width = p_dbl(5, doc = "Width of the plot."),
    height = p_dbl(5, doc = "Height of the plot."),
    file_type = p_choice(
      ".png",
      c(".png", ".pdf"),
      doc = "Plot type to save. Might be expanded to other file types."
    ),
    unit = p_choice(
      "in",
      c("in", "px", "cm"),
      doc = "Unit type for `width` and `height`."
    ),
    res = p_int(450L, doc = "Resolution for PNGs."),
    create_dir = p_lgl(
      TRUE,
      doc = "Shall the plot directory be generated recursively."
    )
  )
)

# volcano plot params ----------------------------------------------------------

spec_volcano <- param_spec(
  name = "volcano",
  title = "Wrapper function for volcano plot parameters",
  checker = "Volcano",
  checker_args = alist(dt = NULL),
  label = "volcano params",
  extra_check = quote({
    # labelling needs a label column
    if (
      !is.null(x[["top_features_to_label"]]) && is.null(x[["label_column"]])
    ) {
      return("`top_features_to_label` is set but `label_column` is missing.")
    }
    # column existence in dt (NULL entries are dropped by c())
    if (!is.null(dt)) {
      res <- checkmate::checkDataFrame(dt)
      if (!isTRUE(res)) {
        return(res)
      }
      needed <- c(
        x[["x_axis"]],
        x[["y_axis"]],
        x[["colour"]],
        x[["label_column"]]
      )
      res <- checkmate::checkSubset(needed, choices = names(dt))
      if (!isTRUE(res)) {
        return(sprintf(
          paste(
            "The following columns are referenced in the params but missing",
            "in dt: %s"
          ),
          paste(setdiff(needed, names(dt)), collapse = ", ")
        ))
      }
    }
  }),
  fields = list(
    x_axis = p_chr(
      "log2FC",
      doc = "Column holding the effect size (e.g. `\"log2FC\"`)."
    ),
    y_axis = p_chr(
      "FDR",
      doc = paste(
        "Column holding the raw significance values (e.g. `\"FDR\"`,",
        "`\"fdr\"`, `\"q_value\"`). The function applies `-log10()`",
        "internally."
      )
    ),
    colour = p_chr(
      NULL,
      null_ok = TRUE,
      doc = paste(
        "Column to colour points by (continuous gradient). If `NULL`, points",
        "are coloured by `x_axis`."
      )
    ),
    label_column = p_chr(
      NULL,
      null_ok = TRUE,
      doc = paste(
        "Column holding feature labels. Required if `top_features_to_label`",
        "is set."
      )
    ),
    top_features_to_label = p_int(
      NULL,
      range = "[1,)",
      null_ok = TRUE,
      integerish = TRUE,
      doc = paste(
        "Number of features to label, ranked by `y_axis` ascending (most",
        "significant first)."
      )
    )
  )
)
