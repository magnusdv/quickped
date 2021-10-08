suppressPackageStartupMessages(
  library(pedtools)
)

relab = function(x)
  relabel(x, "asPlot")

swpSx = function(x, ids)
  relab(swapSex(x, ids, verbose = FALSE))

# Alternative version of pedtools::readPed()
readPed2 = function(pedfile, sep = "\t", ...) {
  df = read.table(pedfile, sep = sep,  header = TRUE, colClasses = "character", check.names = FALSE, quote = NULL)
  as.ped(df)
}

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
  "Tutankhamun" = readPed2("data/tutankhamun.ped")
)


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
    "Tutankhamun"
  )
)

