# saving plots -----------------------------------------------------------------

suppressPackageStartupMessages(library(ggplot2))

## random data and plots -------------------------------------------------------

set.seed(123L)

random_data <- data.table::data.table(
  x = 1:10,
  y = rnorm(10)
)

random_data_2 <- data.table::data.table(
  x = c(rep("a", 3), rep("b", 3), rep("c", 3)),
  y = rnorm(9)
)

p1 <- ggplot(data = random_data, mapping = aes(x = x, y = y)) + geom_point()
p2 <- ggplot(data = random_data_2, mapping = aes(x = x, y = y)) + geom_boxplot()

## tests -----------------------------------------------------------------------

### single plots ---------------------------------------------------------------

dir_path <- tempdir()
dir_path_2 <- file.path(dir_path, "test")

# png
suppressMessages(save_plot(p1, path = dir_path))

# pdf
suppressMessages(save_plot(
  p1,
  path = dir_path,
  plot_params = params_plots(file_type = ".pdf")
))

# different file name
save_plot(plot = p1, file_name = "actual_name.png", path = dir_path)

expect_true(
  current = "p1.png" %in% list.files(dir_path),
  info = paste("saving to unnamed PNG")
)
expect_true(
  current = "p1.pdf" %in% list.files(dir_path),
  info = paste("saving to unnamed PDF")
)
expect_true(
  current = "actual_name.png" %in% list.files(dir_path),
  info = paste("saving to named PNG")
)

# without sub directory creation
expect_error(
  current = save_plot(
    p1,
    path = dir_path_2,
    plot_params = params_plots(create_dir = FALSE)
  ),
  info = paste("error if directory does not exist")
)

suppressMessages(save_plot(
  p1,
  path = dir_path_2,
  plot_params = params_plots(create_dir = TRUE)
))

expect_true(
  current = "p1.png" %in% list.files(dir_path_2),
  info = paste("saving to unnamed PNG in subdirectory")
)

# unlink this
unlink(dir_path_2, recursive = TRUE)

### multiple plots -------------------------------------------------------------

plot_list <- list(
  p_ls_1 = p1,
  p_ls_2 = p2
)

save_plot_ls(plot_ls = plot_list, path = dir_path)

save_plot_ls(
  plot_ls = plot_list,
  path = dir_path,
  plot_params = params_plots(file_type = '.pdf')
)

expect_error(
  save_plot_ls(
    p1,
    path = dir_path_2,
    plot_params = params_plots(create_dir = FALSE)
  ),
  info = paste("error when single plot is provided")
)

expect_true(
  current = all(sprintf("%s.png", names(plot_list)) %in% list.files(dir_path)),
  info = paste("saving from plot list to PNG")
)

expect_true(
  current = all(sprintf("%s.pdf", names(plot_list)) %in% list.files(dir_path)),
  info = paste("saving from plot list to PDF")
)
