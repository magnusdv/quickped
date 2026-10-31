# Keep shiny::runApp(".") and shinyapps.io source deployments working.
# Developing the app does not require installing the quickped package first.
shiny::shinyAppDir(file.path("inst", "app"))
