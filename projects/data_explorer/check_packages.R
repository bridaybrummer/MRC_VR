# Check and install required packages for Data Explorer
packages <- c("shiny", "bslib", "arrow", "data.table", "plotly", "DT", "labelled", "here")

cat("Checking required packages for Data Explorer...\n\n")

for (p in packages) {
  if (requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("  ✓ %s - installed\n", p))
  } else {
    cat(sprintf("  ✗ %s - MISSING (installing...)\n", p))
    install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}

cat("\nDone!\n")
