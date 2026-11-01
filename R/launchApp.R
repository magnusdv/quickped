#' Launch QuickPed
#'
#' Launch the QuickPed application for creating and analysing pedigrees.
#'
#' @param launch.browser Whether to open the app in a browser. Default: TRUE.
#' @param ... Further arguments passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()].
#'
#' @examples
#'
#' if(interactive()) launchApp()
#'
#' @export
launchApp = function(launch.browser = TRUE, ...) {
  appDir = system.file("shiny", package = "quickped", mustWork = TRUE)
  suppressPackageStartupMessages(
    shiny::startApp(appDir, launch.browser = launch.browser, ...))
}
