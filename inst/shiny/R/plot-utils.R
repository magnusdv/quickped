

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

  if(!is.null(hide <- pedigree$hidelabs)) {
    newdat$hidelabs = idMap[hide]
  }

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

  for(sty in c("hatched", "carrier", "aff", "dashed", "deceased", "proband"))
    if(!is.null(styles[[sty]]))
      newdat[[sty]] = idMap[styles[[sty]]]

  newdat
}


breakLabs = function(labs, breakAt = "  ") {
  if(!length(labs))
    return(labs)
  names(labs) = gsub(breakAt, "\n", labs)
  labs
}


checkNewLabs = function(labs) {
  if(dup <- anyDuplicated.default(labs))
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

  # Coefficients
  coefs = NULL
  if(mode == "noninbred") {
    ids = ids[ribd::inbreeding(ped, ids) == 0]
    if(length(ids) >=2)
      coefs = kappaIBD(ped, ids, inbredAction = 0, simplify = FALSE)
  }
  else if(length(ids) >= 2) {
    coefs = identityCoefs(ped, ids, simplify = FALSE)
    coefs$k0 = coefs$D2 + coefs$D4 + coefs$D6 + coefs$D9
    coefs$k1 = coefs$D3 + coefs$D5 + coefs$D8
    coefs$k2 = coefs$D1 + coefs$D7
  }

  # Axis labels
  if(mode == "noninbred") {
    xlab = expression(kappa[0])
    ylab = expression(kappa[2])
  } else {
    xlab = expression(Delta[2]+Delta[4]+Delta[6]+Delta[9])
    ylab = expression(Delta[1]+Delta[7])
  }

  # Reduce cex if many points
  cex = max(1.5 - 0.1*(NROW(coefs) %/% 10), 0.8)

  # Triangle plot
  gg = showInTriangle(coefs, cex = cex, cexPoint = 1.6, cexText = 1.6, col = "blue",
                      plotType = "ggplot2", xlab = xlab, ylab = ylab)

  if(mode == "inbred")
    gg = gg + ggplot2::theme(axis.title.y = element_text(angle = 90))

  # text(.45, .45, 'inadmissible region', cex = 1.2, srt = -45)

  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)

  # Pedigree in top-right corner (do this first)
  par(fig = c(.49, .98, .49, .98))
  tryCatch(plot(ped, autoScale = TRUE, hatched = ids, ...),
           error = function(e) return(invisible()))
  par(new = TRUE)
  print(gg, newpage = FALSE)
}

