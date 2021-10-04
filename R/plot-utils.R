
# Main pedigree plot function
plotPed = function(pedData, plotargs, selected = NULL, addBox = FALSE) {
  dat = tryCatch(
    plot(pedData$ped,
         aff        = pedData$aff,
         carrier    = pedData$carrier,
         deceased   = pedData$deceased,
         twins      = pedData$twins,
         labs       = breakLabs,
         col        = list(red = selected),
         cex        = plotargs$cex,
         symbolsize = plotargs$symbolsize,
         margins    = plotargs$mar),
    error = function(e) {
      msg = conditionMessage(e)
      if(grepl("reduce cex", msg))
        msg = "Plot region is too small"
      stop(msg)
    }
  )

  if(addBox)
    box("outer", col = 1)

  # Return plot object for storage
  dat
}


# Current plot labels in the order plotted
getPlotOrder = function(ped, plist, perGeneration = FALSE) {

  # Index of each indiv, listed by generation
  idxList = lapply(seq_along(plist$n), function(i) plist$nid[i, 1:plist$n[i]])

  # Check for dups
  dups = duplicated(unlist(idxList))

  if(any(dups)) {
    # Generation number of each (used to remove dups)
    g = rep(seq_along(plist$n), plist$n)

    # Reduce
    g2 = g[!dups]

    # Create new idxList
    idxList = split(unlist(idxList)[!dups], unlist(g2))
  }

  if(perGeneration)
    lapply(idxList, function(v) labels(ped)[v])
  else
    labels(ped)[unlist(idxList)]
}


# Prepare data for updating labels in plot
updateLabelsData = function(currData, old, new, reorder = FALSE) {

  newtw = currData$twins
  newtw$id1 = new[match(newtw$id1, old)]
  newtw$id2 = new[match(newtw$id2, old)]

  list(ped = relabel(currData$ped, old = old, new = new, reorder = reorder),
       aff = new[match(currData$aff, old)],
       carrier = new[match(currData$carrier, old)],
       deceased = new[match(currData$deceased, old)],
       twins = newtw)
}

breakLabs = function(x, breakAt = "  ") {
  labs = labels(x)
  names(labs) = sub(breakAt, "\n", labs)
  labs
}
