# Prepare pedplot arguments from the current labels and styles
pedplotArgs = function(ped, labs, styles, selected, textAnnot, miscarriage,
                      cex, symbolsize, margins) {
  ids = labels(ped)
  # The old label vector stores display text in its names
  txt = names(labs)
  txt[labs == ""] = ""
  selected = ids %in% selected
  dashed = ids %in% styles$dashed

  col = rep("black", length(ids))
  col[selected] = "red"
  lwd = rep(1, length(ids))
  lwd[dashed] = 1.5
  lwd[selected] = 3

  cex = checknum(cex, "Cex", min = 0.01, max = 10)
  symbolsize = checknum(symbolsize, "Symbols", min = 0.01, max = 10)
  margins = checknum(margins, "Margins", min = 0, max = 10)

  list(labs = setNames(txt, ids), col = col, fill = styles$fill %||% NA,
       lty = 1L + dashed, lwd = lwd, hatched = styles$hatched, hatchDensity = 20,
       carrier = styles$carrier, deceased = styles$deceased, proband = styles$proband,
       miscarriage = miscarriage, textAnnot = formatAnnot(textAnnot, cex - 0.2),
       cex = cex, symbolsize = symbolsize, margins = margins)
}


# Scale and draw on the current device, reusing the cached alignment
drawPedplot = function(align, args) {
  scale = do.call(pedplot::scalePed, c(list(align = align), args))
  parts = do.call(pedplot::partsPed, c(list(align = align, scale = scale), args))
  pedplot::drawPed(parts, scale)
  parts$symbols
}


# Keep all plotted copies linked to their original individual
pedplotPositions = function(symbols) {
  data.frame(x = symbols$x, y = symbols$y + symbols$height / 2,
             idInt = symbols$idx)
}


# Number primary copies by generation and horizontal position
pedplotLabels = function(align, new) {
  nodes = align$nodes
  nodes = nodes[nodes$primary, ]
  nodes = nodes[order(nodes$gen, nodes$x), ]
  labs = as.character(seq_len(nrow(nodes)))
  if(new == "generations") {
    gen = match(nodes$gen, unique.default(nodes$gen))
    labs = paste0(as.roman(gen), "-", sequence(tabulate(gen)))
  }
  setNames(labs, nodes$id)[align$input$id]
}
