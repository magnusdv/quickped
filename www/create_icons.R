library(pedtools)

plotIcon = function(fname, ped = pedtools::singleton(1), pixh = 32, pixw = pixh,
                    symbolsize = 1.5, open = F, mar = 0, bg = NA, ...) {
  svg(fname, width = pixw/72, height = pixh/72, bg = bg)
  par(mar = c(0,0,0,0))
  plot(ped, labs = NULL, margin = mar, symbolsize = symbolsize, ...)
  dev.off()

 if(open)
   browseURL(fname)
}

open = F
plotIcon("open.svg")
plotIcon("hatched.svg", hatched = 1, open = open)
plotIcon("carrier.svg", carrier = 1, open = open)
plotIcon("question.svg", textAnnot = list(inside = "?"), open = open)
plotIcon("deceased.svg", deceased = 1, open = open)
plotIcon("dashed.svg", lty = 2, open=T, open = open)

plotIcon("fill-white.svg", fill = "0", open = open)
plotIcon("fill-black.svg", fill = "1", open = open)
plotIcon("fill-red.svg", fill = "2", open = open)
plotIcon("fill-green.svg", fill = "3", open = open)
plotIcon("fill-blue.svg", fill = "4", open = open)
plotIcon("fill-cyan.svg", fill = "5", open = open)
plotIcon("fill-magenta.svg", fill = "6", open = open)
plotIcon("fill-yellow.svg", fill = "7", open = open)
plotIcon("fill-pink.svg", fill = "pink", open = open)
plotIcon("fill-gray.svg", fill = "8", open = open)

plotIcon("sex-male.svg", ped = pedtools::singleton(1, sex = 1), open = open)
plotIcon("sex-female.svg", ped = pedtools::singleton(1, sex = 2), open = open)
plotIcon("sex-unknown.svg", ped = pedtools::singleton(1, sex = 0), open = open)
