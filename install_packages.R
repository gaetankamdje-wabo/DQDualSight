# Install CRAN packages required by the app
# Run once:
#   source('install_packages.R')

pkgs <- c('shiny', 'bs4Dash', 'DT', 'readxl', 'jsonlite', 'stringr', 'dplyr', 'lubridate', 'data.table', 'shinyjs', 'shinyWidgets', 'rintrojs', 'waiter', 'shinyjqui')
to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
