source(file.path("R", "draw_figure.R"))

dir.create("output", showWarnings = FALSE, recursive = TRUE)
output_file <- file.path("output", "uses_of_mortality_data.png")

draw_mortality_figure(output_file)
message("Saved: ", normalizePath(output_file))
