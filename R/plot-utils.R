

# Prepare data for updating labels in plot
updateLabelsData = function(currData, old = NULL, new, .alignment = NULL) {
  ped = currData$ped
  reorder = FALSE

  # If new = "asPlot" or "generations" (hack needed since pedigree is always reordered)
  if(is.null(old)) {
    idMap = relabel(ped, old = NULL, new = new, .alignment = .alignment, returnLabs = TRUE)
    newped = relabel(ped, old = names(idMap), new = idMap, reorder = TRUE)
  }
  else {
    newped = relabel(ped, old = old, new = new, reorder = FALSE)
    idMap = setNames(newped$ID, ped$ID)
  }

  newtw = currData$twins
  newtw$id1 = idMap[newtw$id1]
  newtw$id2 = idMap[newtw$id2]

  list(ped = newped,
       aff = idMap[currData$aff],
       carrier = idMap[currData$carrier],
       deceased = idMap[currData$deceased],
       twins = newtw)
}

breakLabs = function(x, breakAt = "  ") {
  labs = x$ID
  names(labs) = gsub(breakAt, "\n", labs)
  labs
}


plotKappa = function(ped, kappa, ids, col = "blue") {
  inbr = ribd::inbreeding(ped, ids)
  if(length(ids))
    ids = ids[inbr == 0]

  # Reduce cex if many points
  cex = 1.5 - 0.1 * (nrow(kappa) %/% 10)
  cex = max(cex, 0.8)

  # Triangle plot
  gg = showInTriangle(kappa, cex = cex, cexPoint = 1.6, cexText = 1.6,
                      col = "blue", plotType = "ggplot2")
  # text(.45, .45, 'inadmissible region', cex = 1.2, srt = -45)

  # Pedigree in top-right corner (do this first)
  par(fig = c(.48, .98, .48, .98))
  plot(ped, hatched = ids)
  par(new = TRUE)
  print(gg, newpage = FALSE)
}

