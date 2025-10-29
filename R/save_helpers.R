# saving helpers ---------------------------------------------------------------

#' General helper to save all types of plots to file
#'
#' @param plot The plot you want to save to disk.
#' @param file_name Optional String. If not provided, the plot will be named
#' after the R variable.
#' @param path Directory. Where to save the plot to.
#' @param plot_params List. Output of [bixverse.plots::params_plots()]. A list
#' with the following elements:
#' \itemize{
#'  \item width - Width of the plot.
#'  \item height - Height of the plot.
#'  \item file_type - Which file type.
#'  \item unit - Which unit do width and height describe.
#'  \item res - Resolution for PNG plots
#' }
#'
#' @return Saves the plot to disk and returns `invisible`.
#'
#' @export
save_plot <- function(
  plot,
  file_name = NULL,
  path,
  plot_params = params_plots()
) {
  # checks
  checkmate::qassert(file_name, c("S1", "0"))
  assertPlotParams(plot_params)

  # directory
  if (!dir.exists(path) & plot_params$create_dir) {
    message("Creating directory")
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  } else {
    checkmate::assertDirectory(path)
  }

  # file name
  if (is.null(file_name)) {
    message("No file name supplied. Filename will be set to R object name.")
    file_name <- paste0(deparse(substitute(plot)), plot_params$file_type)
  }

  f_path <- file.path(path, file_name)

  # different file types
  if ("gg" %in% class(plot) | "ggExtraPlot" %in% class(plot)) {
    ## ggplot scenario - easiest
    with(
      plot_params,
      ggplot2::ggsave(
        filename = f_path,
        plot = plot,
        width = width,
        height = height,
        units = unit,
        dpi = res
      )
    )
  } else {
    switch(
      plot_params$file_type,
      .pdf = save_pdf(plot = plot, f_path = f_path, plot_params = plot_params),
      .png = save_png(plot = plot, f_path = f_path, plot_params = plot_params)
    )
  }

  invisible()
}

#' Save a list of plots
#'
#' @param plot_ls Named list of plots. The names will serve as file names (the
#' extension will be added by the function)
#' @param path Directory. Where to save the plot to.
#' @param plot_params List. Output of [bixverse.plots::params_plots()]. A list
#' with the following elements:
#' \itemize{
#'  \item width - Width of the plot.
#'  \item height - Height of the plot.
#'  \item file_type - Which file type.
#'  \item unit - Which unit do width and height describe.
#'  \item res - Resolution for PNG plots
#' }
#'
#' @return Saves the plots to disk and returns `invisible`.
#'
#' @export
save_plot_ls <- function(plot_ls, path, plot_params = params_plots()) {
  # checks
  checkmate::assertList(plot_ls, names = "named")
  checkmate::qassert(path, "S1")
  assertPlotParams(plot_params)

  # update the names
  names(plot_ls) <- paste0(names(plot_ls), plot_params$file_type)

  purrr::iwalk(plot_ls, \(p, p_name) {
    save_plot(
      plot = p,
      file_name = p_name,
      path = path,
      plot_params = plot_params
    )
  })

  invisible()
}

## helpers ---------------------------------------------------------------------

#' Helper function to save various plot types to PNG
#'
#' @param plot The plot you want to save to disk.
#' @param f_path File path. The path to which you want to save the PNG
#' @param plot_params List. Output of [bixverse.plots::params_plots()].
#'
#' @return Saves the PNG to disk.
save_png <- function(plot, f_path, plot_params) {
  # checks
  checkmate::assertPathForOutput(f_path, overwrite = TRUE)
  assertPlotParams(plot_params)

  # main function
  with(plot_params, {
    png(
      f_path,
      width = width,
      height = height,
      unit = unit,
      res = res
    )
    if (checkmate::test_class(plot, "Heatmap")) {
      if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
        dev.off() # close the PNG device before throwing error
        stop(
          paste(
            "Package 'ComplexHeatmap' is required to save Heatmap objects.",
            "Please install it with: BiocManager::install('ComplexHeatmap')"
          )
        )
      }
      ComplexHeatmap::draw(plot)
    } else if (checkmate::test_class(plot, "pheatmap")) {
      if (!requireNamespace("pheatmap", quietly = TRUE)) {
        dev.off()
        stop(
          "Package 'pheatmap' is required to save pheatmap objects.",
          "Please install it with: install.packages('pheatmap')"
        )
      }
      grid::grid.newpage()
      grid::grid.draw(plot$gtable)
    } else if (checkmate::test_class(plot, "upset")) {
      if (!requireNamespace("UpSetR", quietly = TRUE)) {
        dev.off()
        stop(
          paste(
            "Package 'UpSetR' is required to save upset objects.",
            "Please install it with: install.packages('UpSetR')"
          )
        )
      }
      print(plot)
    } else {
      plot
    }
    dev.off()
  })
}

#' Helper function to save various plot types to PDF
#'
#' @param plot The plot you want to save to disk.
#' @param f_path File path. The path to which you want to save the PDF.
#' @param plot_params List. Output of [bixverse.plots::params_plots()].
#'
#' @return Saves the PDF to disk.
save_pdf <- function(plot, f_path, plot_params) {
  # checks
  checkmate::assertPathForOutput(f_path, overwrite = TRUE)
  assertPlotParams(plot_params)

  # main function
  with(plot_params, {
    pdf(
      f_path,
      width = width,
      height = height
    )
    if (checkmate::test_class(plot, "Heatmap")) {
      if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
        dev.off() # close the PNG device before throwing error
        stop(
          paste(
            "Package 'ComplexHeatmap' is required to save Heatmap objects.",
            "Please install it with: BiocManager::install('ComplexHeatmap')"
          )
        )
      }
      ComplexHeatmap::draw(plot)
    } else if (checkmate::test_class(plot, "pheatmap")) {
      if (!requireNamespace("pheatmap", quietly = TRUE)) {
        dev.off()
        stop(
          "Package 'pheatmap' is required to save pheatmap objects.",
          "Please install it with: install.packages('pheatmap')"
        )
      }
      grid::grid.newpage()
      grid::grid.draw(plot$gtable)
    } else if (checkmate::test_class(plot, "upset")) {
      if (!requireNamespace("UpSetR", quietly = TRUE)) {
        dev.off()
        stop(
          paste(
            "Package 'UpSetR' is required to save upset objects.",
            "Please install it with: install.packages('UpSetR')"
          )
        )
      }
      print(plot)
    } else {
      plot
    }
    dev.off()
  })
}
