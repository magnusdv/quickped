

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

# Old version; not used
plotKappa = function(k, ids, col = "blue") {
  showInTriangle(k, cex = 2.5, lwd = 3.5, col = col, cexPoint = 1.6,
                 cexText = 1.6, labels = FALSE)
  lab = paste(ids, collapse = " - ")
  adj = c(.5, -1.25)
  n = nchar(lab)
  if(k[1] == 0 && n >= 20) adj[1] = 0.25
  else if(n > 46 - 30*k[1]) adj[1] = 0.5 + (1 - (46 - 30*k[1])/n)

  text(k[1], k[3], lab, cex = 1.5, col = col, adj = adj)
  text(.45, .45, 'inadmissible region', cex = 1.2, srt = -45)
}

