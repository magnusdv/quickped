# Loading the package must not alter the caller's directory.
originalWd = getwd()
library(quickped)
stopifnot(identical(getwd(), originalWd))

# Constructing the app must also preserve the caller's directory.
app = quickpedApp()
stopifnot(inherits(app, "shiny.appobj"), identical(getwd(), originalWd))

appDir = system.file("app", package = "quickped", mustWork = TRUE)
stopifnot(all(file.exists(file.path(appDir,
  c("app.R", "GA.html", "R/peds-builtin.R", "www/custom.css", "data/habsburg.ped")))))

# Verify the bundled app can construct its UI and read its built-in pedigrees.
# This exercises relative asset/data paths from outside the source checkout.
response = app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
  SCRIPT_NAME = "", QUERY_STRING = "", HTTP_HOST = "localhost"))
stopifnot(response$status == 200L)
