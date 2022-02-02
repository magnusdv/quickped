suppressPackageStartupMessages(
  library(pedtools)
)

# Alternative version of pedtools::readPed()
readPed2 = function(pedfile) {

  # Read first line
  line1 = readLines(pedfile, n = 1L, encoding = "UTF-8")

  # Determine separator
  if(grepl("\t", line1))
    sep = "\t"
  else
    sep = "" # whitespace

  # Is the first line a header line?
  header = grepl("id", line1, ignore.case = TRUE) && grepl("sex", line1, ignore.case = TRUE)

  # Read table
  df = read.table(pedfile, sep = sep, header = header, colClasses = "character",
                  check.names = FALSE, quote = "\"", encoding = "UTF-8")

  # At least 3 rows?
  if(nrow(df) < 3)
    stop("Only ", nrow(df), " rows found in the pedigree file; expected at least 3.")

  # Remove first column if family ID
  fstname = tolower(names(df)[1])
  hasFamid = fstname %in% c("fid", "famid") || identical(df[1,1], df[2,1])
  if(hasFamid) {
    if(length(unique.default(df[[1]])) > 1)
      stop("The loaded pedigree has multiple components. Only connected pedigrees are allowed.")
    df[[1]] = NULL
  }

  # Now check columns
  NC = ncol(df)
  if(NC < 3)
    stop("Only ", ncol(df), " columns found in the pedigree file; expected at least 4 (id, fid, mid, sex).",
         "\n\nColumn separator guessed: ", if(sep == "\t") "tab" else "<whitespace>")

  # If 5th column present, interpret as affection status
  if(NC >= 5) {
    hasAff = TRUE
    affcodes = df[[5]]
    df = df[, 1:4]
  }
  else
    hasAff = FALSE

  names(df) = c("id", "fid", "mid", if(NC > 3) "sex")

  # Convert to ped object
  ped = as.ped(df)

  if(!is.ped(ped))
    stop("The loaded pedigree has multiple components. Only connected pedigrees are allowed.")

  # Affected individuals
  aff = if(hasAff) ped$ID[affcodes == 2] else character(0)

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
  "Uncle/nephew" = addSon(nuclearPed(2), 4, verbose = FALSE),
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
    "Uncle/nephew",
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

