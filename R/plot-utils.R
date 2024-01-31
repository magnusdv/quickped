

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


plotKappa = function(ped, ids, mode = "noninbred", col = "blue") {

  if(!length(ids))
    ids = labels(ped)

  if(mode == "noninbred") {
    inbr = ribd::inbreeding(ped, ids)
    ids = ids[inbr == 0]
    if(length(ids) >= 2)
      coefs = kappaIBD(ped, ids, inbredAction = 0, simplify = FALSE)
    else
      # hack! TODO: Remove when showInTriangle works with NULL
      coefs = data.frame(kappa0 = -1, kappa2 = -1)
    xlab = expression(kappa[0])
    ylab = expression(kappa[2])
  }
  else {
    j = identityCoefs(ped, ids, simplify = FALSE)
    j$k0 = j$D2 + j$D4 + j$D6 +j$D9
    j$k1 = j$D3 + j$D5 + j$D8
    j$k2 = j$D1 + j$D7
    coefs = j
    xlab = expression(Delta[2]+Delta[4]+Delta[6]+Delta[9])
    ylab = expression(Delta[1]+Delta[7])
    }


  # Reduce cex if many points
  cex = 1.5 - 0.1 * (nrow(coefs) %/% 10)
  cex = max(cex, 0.8)

  # Triangle plot
  gg = showInTriangle(coefs, cex = cex, cexPoint = 1.6, cexText = 1.6, col = "blue",
                      plotType = "ggplot2", xlab = xlab, ylab = ylab)

  # TODO fix view for hack
  if("kappa0" %in% names(coefs) && coefs$kappa0[1] == -1)
    gg = suppressMessages(gg + coord_fixed(ratio = 1, clip = "off", xlim = c(0,1), ylim = c(0,1)))

  if(mode == "inbred")
    gg = gg + ggplot2::theme(axis.title.y = element_text(angle = 90))

  # text(.45, .45, 'inadmissible region', cex = 1.2, srt = -45)

  # Pedigree in top-right corner (do this first)
  par(fig = c(.49, .98, .49, .98))
  tryCatch(plot(ped, autoScale = T, hatched = ids),
           error = function(e) return(invisible()))
  par(new = TRUE)
  print(gg, newpage = FALSE)
}

