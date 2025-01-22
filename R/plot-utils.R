

# Prepare data for updating labels
updateLabelsData = function(pedigree, styles, textAnnot, new, .alignment = NULL) {
  .debug("updateLabelsData")

  newdat = list()

  ped = pedigree$ped

  if(identical(new, "asPlot") || identical(new, "generations")) {
    # hack needed because of reordering (??)
    idMap = relabel(ped, new = new, .alignment = .alignment, returnLabs = TRUE)
    newped = relabel(ped, new = idMap, reorder = TRUE)
  }
  else {
    new = checkNewLabs(new)
    newped = relabel(ped, new = new, reorder = FALSE)
    idMap = setNames(labels(newped), labels(ped))
  }

  newdat$ped = newped

  if(!is.null(twins <- pedigree$twins)) {
    twins$id1 = idMap[twins$id1]
    twins$id2 = idMap[twins$id2]
    newdat$twins = twins
  }

  if(!is.null(misc <- pedigree$miscarriage)) {
    newdat$miscarriage = idMap[misc]
  }

  if(!is.null(names(fill <- styles$fill))) {
    names(fill) = idMap[names(fill)]
    newdat$fill = fill
  }

  if(!is.null(textAnnot))
    newdat$textAnnot = lapply(textAnnot, function(v)
      setNames(v, idMap[names(v)]))

  for(sty in c("hatched", "carrier", "aff", "dashed", "deceased"))
    if(!is.null(styles[[sty]]))
      newdat[[sty]] = idMap[styles[[sty]]]

  newdat
}


breakLabs = function(x, breakAt = "  ") {
  labs = labels(x)
  names(labs) = gsub(breakAt, "\n", labs)
  labs
}


checkNewLabs = function(labs) {
  if(dup <- anyDuplicated(labs))
    stop2("Duplicated ID label: ", labs[dup])

  if(0 %in% labs)
    stop2('"0" cannot be used as label')

  if("" %in% labs)
    stop2("Empty labels are not allowed")

  labs
}


formatAnnot = function(textAnnot, cex, font = 2, col = "blue") {
  if(is.null(textAnnot))
    return(NULL)
  lapply(textAnnot, function(b) list(b, cex = cex, font = font, col = col))
}

plotKappa = function(ped, ids, mode = "noninbred", ...) {

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
  tryCatch(plot(ped, autoScale = T, hatched = ids, ...),
           error = function(e) return(invisible()))
  par(new = TRUE)
  print(gg, newpage = FALSE)
}

