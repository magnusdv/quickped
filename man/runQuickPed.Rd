\name{runQuickPed}
\alias{runQuickPed}
\alias{quickpedApp}
\title{Create or Run the QuickPed Application}
\usage{
quickpedApp()

runQuickPed(...)
}
\arguments{
\item{...}{Arguments passed to \code{shiny::runApp()}, such as \code{host},
\code{port}, and \code{launch.browser}.}
}
\description{
QuickPed is an interactive Shiny application for drawing and analysing pedigrees.
The application and its assets are bundled with the package.
}
\value{
\code{quickpedApp()} returns a Shiny application object without starting a server.
\code{runQuickPed()} starts the application and returns when it is stopped.
}
\examples{
if (interactive()) {
  runQuickPed()
}
}
