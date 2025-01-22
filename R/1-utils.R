### Helper functions for the QuickPed app

stop2 = function (...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

`%||%` = function(x,y)
  if(is.null(x)) y else x

mylink = function(text, href, .noWS = "outside", ...) {
  if(missing(href))
    href = text
  shiny::a(text, href = href, .noWS = .noWS, target = "_blank", ...)
}

.myintersect = function(x, y)
  y[match(x, y, 0L)]

.mysetdiff = function(x, y)
  unique.default(x[match(x, y, 0L) == 0L])


checknum = function(a, var, min = 0, max = Inf) { #print("checknum")
  if(is.na(a) || a < min || a > max)
    stop2(sprintf("Invalid value of '%s'", var))
  a
}


updateTwins = function(ped, twins, ids) {
  if(length(ids) != 2)
    stop2("To change twin status, please select exactly 2 individuals")

  ids = sort.default(ids)
  id1 = ids[1]
  id2 = ids[2]

  if(!identical(parents(ped, id1), parents(ped, id2)))
    stop2("Twins must have the same parents")
  if(id1 %in% founders(ped))
    stop2("Founders cannot be twins")

  sameSex = getSex(ped, id1) == getSex(ped, id2)

  # Check if already twins (gives 0 if empty)
  rw = match(TRUE, nomatch = 0, twins$id1 == id1 & twins$id2 == id2)

  # If new: add. Otherwise: cycle MZ -> DZ -> ? -> 0
  if(rw == 0)
    twins = rbind(twins, data.frame(id1 = id1, id2 = id2, code = 2 - sameSex))
  else if (twins$code[rw] < 3)
    twins$code[rw] = twins$code[rw] + 1
  else
    twins = twins[-rw, , drop = FALSE]

  if(nrow(twins)) twins else NULL
}

addSib = function(x, id, sex = 1, side = c("right", "left")) {

  if(length(id) > 1)
    stop2("To add a sibling, please select exactly one individual. Current selection: ", sortIds(x, id))

  if(!is.ped(x))
    stop2("Cannot add sibling to disconnected pedigree")

  if(id %in% founders(x))
    x = addParents(x, id, verbose = FALSE)

  newped = addChild(x, parents(x, id), sex = sex, verbose = FALSE)

  # Reorder so that new sib comes directly before or after (default) `id`
  idInt = internalID(x, id)
  n = length(x$ID)
  ord = switch(match.arg(side),
               left = c(seq_len(idInt-1), n+1, idInt:n),
               right = c(seq_len(idInt), n+1, if(idInt < n) seq.int(idInt+1, n)))

  reorderPed(newped, ord)
}

addPar = function(x, ids) {
  n = length(ids)
  pars = ids[-1] # parents, if more than 1 is selected
  parsex = getSex(x, pars)
  fa = mo = NULL
  if(n == 3) {
    if(parsex[1] == 1 && parsex[2] == 2)      {fa = pars[1]; mo = pars[2]}
    else if(parsex[1] == 2 && parsex[2] == 1) {fa = pars[2]; mo = pars[1]}
    else
      stop2("Incompatible sex of selected parents: ", ids[2:3])
  }
  else if(n == 2) {
    if(parsex[1] == 1) fa = ids[2]
    else if(parsex[1] == 2) mo = ids[2]
    else
      stop2("Cannot use individuals of uknown sex as parent: ", ids[2])
  }
  else if(n != 1)
    stop2("Too many individuals selected")

  addParents(x, ids[1], father = fa, mother = mo, verbose = F)
}

# No reactives here! `dat` contains all elements
removeSel = function(dat, ids, updown) {
  newped = removeIndividuals(dat$ped, ids, remove = updown, verbose = FALSE)

  if(is.null(newped))
    stop2(sprintf("Removing %s would leave a empty pedigree",
                  ifelse(length(ids) == 1, sprintf("'%s'", ids), "these individuals")))

  if(is.pedList(newped))
    stop2(sprintf("Removing %s would disconnect the pedigree, which is currently not supported",
                  ifelse(length(ids) == 1, sprintf("'%s'", ids), "these individuals")))

  newlabs = labels(newped)

  # Twin data
  newtw = dat$twins
  newtw = newtw[newtw$id1 %in% newlabs & newtw$id2 %in% newlabs, , drop = FALSE]

  # Miscarriage
  misc = dat$miscarriage
  newmisc = .myintersect(misc, newlabs)

  # Styles except 'fill'
  sty = c("hatched", "carrier", "dashed", "deceased") |> setNames(nm = _)
  newstyles = lapply(sty, function(s) .myintersect(dat[[s]], newlabs))

  # Style 'fill'
  fill = dat$fill
  newstyles$fill = fill[.myintersect(names(fill), newlabs)]

  # Text annotation
  newText = lapply(dat$textAnnot, function(v) v[.myintersect(names(v), newlabs)])
  newText = newText[lengths(newText) > 0]

  # Collect everything in one list
  newdat = c(list(ped = newped, twins = newtw, miscarriage = newmisc),
             newstyles, list(textAnnot = newText))

  newdat
}

sortIds = function(x, ids) {
  intern = internalID(x, ids)
  ids[order(intern)]
}


# Update vector x by vector y
# Alternative syntax: Set value `val` for all elements named by y
modifyVec = function(x, y, val = NULL) {
  if(!length(y))
    return(x)

  # If x has no names, remove
  if(is.null(names(x)))
    length(x) = 0

  if(!is.null(val))
    y = rep(val, length(y)) |> setNames(y)

  res = c(x, y)
  res[!duplicated.default(names(res), fromLast = TRUE)]
}


changeSex = function(ped, ids, sex, twins = NULL) {

  if(sex == 0) {
    if(!all(ids %in% leaves(ped)))
      stop2("Only individuals without children can have unknown sex")
    newped = setSex(ped, ids, sex = 0)
    return(newped)
  }

  # By now: sex is 1 or 2
  currentSex = getSex(ped, ids)

  newped = ped |>
    swapSex(ids[currentSex == (3-sex)], verbose = FALSE) |>
    setSex(ids[currentSex == 0], sex = sex)

  # Catch discordant swaps for MZ twins
  mz = twins[twins$code == 1, , drop = FALSE]
  if(!is.null(mz) && nrow(mz) > 0) {
    sx1 = getSex(newped, mz$id1)
    sx2 = getSex(newped, mz$id2)
    if(any(sx1 > 0 & sx2 > 0 & sx1 != sx2))
      stop2("Cannot change sex of one MZ twin")
  }

  newped
}
