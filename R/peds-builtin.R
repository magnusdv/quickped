suppressPackageStartupMessages(
  library(pedtools)
)

# Alternative version of pedtools::readPed()
readPed2 = function(pedfile) {
  cls = c("id", "fid", "mid", "sex")

  df = read.table(pedfile, sep = "\t", header = TRUE, colClasses = "character",
                  check.names = FALSE, quote = "\"", encoding = "UTF-8")
  names(df) = nms = tolower(names(df))
  if(!all(cls %in% nms))
    stop("Column not found: ", toString(setdiff(cls, nms)))

  # Convert to ped object
  ped = as.ped(df[cls])

  # Affected column, if present
  aff = if("aff" %in% nms) ped$ID[df$aff == 2] else character(0)

  list(ped = ped, aff = aff)
}


relab = function(x)
  relabel(x, "asPlot")

swpSx = function(x, ids)
  relab(swapSex(x, ids, verbose = FALSE))


BUILTIN_PEDS = list(
  "Trio" = nuclearPed(1),
  "Full siblings" = nuclearPed(2, sex = 1:2),
  "Half siblings (mat)" = halfSibPed(1, 1, sex2 = 2, type = "maternal"),
  "Half siblings (pat)" = halfSibPed(1, 1, sex2 = 2, type = "paternal"),
  "Grandparent" = linearPed(2),
  "Great-grandparent" = linearPed(3),
  "Aunt/niece" = relab(addDaughter(nuclearPed(2, sex = 2), 3, verbose = FALSE)),
  "Uncle/newphew" = addSon(nuclearPed(2), 4, verbose = FALSE),
  "1st cousins"   = swpSx(cousinPed(1), c(3, 8)),
  "2nd cousins"  = swpSx(cousinPed(2), 12),
  "Half 1st cousins"   = swpSx(halfCousinPed(1), c(4,9)),
  "Half 2nd cousins"  = swpSx(halfCousinPed(2), 13),

  "3/4-siblings" = addChildren(addChildren(nuclearPed(2), 3, mother = 5, 1, verbose = FALSE), 4, mother = 5, nch = 1, sex = 2),
  "5/8-siblings" = swpSx(halfSibStack(2), 8),
  "Full sib mating" = fullSibMating(1),
  "Double 1st cousins" = swpSx(doubleFirstCousins(), 10),
  "Double 2nd cousins" = swpSx(doubleCousins(degree1 = 2, degree2 = 2), 18),
  "Quad half 1st cousins" = quadHalfFirstCousins(),

  "Habsburg" = readPed2("data/habsburg.ped"),
  "Jicaque" = readPed2("data/jicaque.ped"),
  "Queen Victoria (haemophilia)" = readPed2("data/haemophilia.ped"),
  "Tutankhamun" = readPed2("data/tutankhamun.ped")
)

# Add carriers of Queen Vic pedigree (all females with non-numerical label)
qv = BUILTIN_PEDS[["Queen Victoria (haemophilia)"]]$ped
BUILTIN_PEDS[["Queen Victoria (haemophilia)"]]$carrier = intersect(females(qv), setdiff(labels(qv), 1:50))

# Plotting parameters for builtin pedigrees
paramsBuiltin = function(choice) {
  switch(choice,
         Habsburg             = list(width = 655, height = 655, cex = 1.1, symbolsize = 1, mar = 2),
         Jicaque              = list(width = 655, height = 655, cex = 1.5, symbolsize = 1, mar = 2),
         `Queen Victoria (haemophilia)`
                              = list(width = 850, height = 430, cex = 0.9, symbolsize = 1.2, mar = 1.5),
         `Quad half 1st cousins`
                              = list(width = 655, height = 430, cex = 1.4, symbolsize = 1.1, mar = 4.5),
         `Double 2nd cousins` = list(width = 655, height = 430, cex = 1.4, symbolsize = 1.1, mar = 3),
         Tutankhamun          = list(width = 430, height = 430, cex = 1.4, symbolsize = 1.1, mar = 4),
         list(width = 430, height = 430, cex = 1.6, symbolsize = 1, mar = 3) # default
  )
}


BUILTIN_CHOICES = list(
  Choose = "",
  `Basic pedigrees` = list(
    "Trio",
    "Full siblings",
    "Half siblings (mat)",
    "Half siblings (pat)",
    "Grandparent",
    "Great-grandparent",
    "Aunt/niece",
    "Uncle/newphew",
    "1st cousins",
    "2nd cousins",
    "Half 1st cousins",
    "Half 2nd cousins"
  ),
  `Complex pedigrees` = list(
    "3/4-siblings",
    "5/8-siblings",
    "Full sib mating",
    "Double 1st cousins",
    "Double 2nd cousins",
    "Quad half 1st cousins"
  ),
  Historic = list(
    "Habsburg",
    "Jicaque",
    "Queen Victoria (haemophilia)",
    "Tutankhamun"
  )
)

