# Launch the Shiny app locally
# Usage:
#   source('run_app.R')

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Please run source('install_packages.R') first to install dependencies.")
}
shiny::runApp('app', launch.browser = TRUE)
