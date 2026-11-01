appR = system.file("shiny/R", package = "quickped")
source(file.path(appR, "1-utils.R"), local = TRUE)
source(file.path(appR, "plot-utils.R"), local = TRUE)
source(file.path(appR, "rcode.R"), local = TRUE)

# Functions normally attached when app.R starts
# library(pedtools)
# library(glue)
addParents = pedtools::addParents
founders = pedtools::founders
getSex = pedtools::getSex
internalID = pedtools::internalID
is.pedList = pedtools::is.pedList
leaves = pedtools::leaves
nuclearPed = pedtools::nuclearPed
parents = pedtools::parents
relabel = pedtools::relabel
removeIndividuals = pedtools::removeIndividuals
setSex = pedtools::setSex
swapSex = pedtools::swapSex
glue = glue::glue
glue_collapse = glue::glue_collapse
