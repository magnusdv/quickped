### Helper functions for the QuickPed app

stop2 = function (...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

.myintersect = function(x, y)
  y[match(x, y, 0L)]

bigHeading = function(x)
  h4(strong(x), .noWS = "before")

midHeading = function(x)
  h5(strong(x), style = "margin-bottom: 0px;")

bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")

errModal = function(..., html = FALSE) {
  args = list(...)
  if(length(args) == 1 && inherits(args, "condition")) {
    mess = conditionMessage(args[[1]])
    if(grepl("reduce cex", mess))
      mess = "Plot region is too small"
  }
  else
    mess = paste(lapply(args, toString), collapse = "")
  if(html)
    mess = HTML(mess)
  showModal(modalDialog(mess, easyClose = TRUE))
}

# range2seq = function(s) {
#   rlist = lapply(strsplit(s, ",")[[1]], function(r) {
#     fromto = as.integer(strsplit(r, "-")[[1]])
#     if(anyNA(fromto))
#       stop2("Illegal range: ", r)
#     seq(min(fromto), max(fromto))
#   })
#   sort.default(unique.default(unlist(rlist)))
# }

textInput2 = function(inputId, value) {
  w  = textInput(inputId, label = NULL, value = value, width = "100%")
  w$children[[2]]$attribs[["style"]] = "padding-top: 1px; padding-bottom: 1px; height: 24px;"
  w
}

pedButton = function(id, label, side = NULL, ...) {
  but = actionButton(id, label, width = "100%",
                     style = "padding-top: 5px; padding-bottom: 5px; padding-left: 0px; padding-right: 0px", ...)

  if(is.null(side))
    return(but)

  switch(side,
   left = column(6, align = "left", style = "padding-right: 3px;", but),
   right = column(6, align = "right", style = "padding-left: 3px;", but)
  )
}

updateTwins = function(twins, ids, code) {
  ids = sort.default(ids)
  id1 = ids[1]
  id2 = ids[2]

  # If previously empty: early return
  if(nrow(twins) == 0)
    return(data.frame(id1 = id1, id2 = id2, code = code))

  # Check if already twins
  rw = match(TRUE, nomatch = 0,
             (twins$id1 == id1 & twins$id2 == id2) | (twins$id2 == id1 & twins$id1 == id2))

  if(rw == 0) {
    # If not: add
    twins = rbind(twins, data.frame(id1 = id1, id2 = id2, code = code))
  }
  else if(twins$code[rw] == code) {
    # If already MZ: remove
    twins = twins[-rw, , drop = FALSE]
  }
  else {
    # Change to MZ
    twins$code[rw] = code
  }

  twins
}

hasMZtwins = function(currData) {
  code = currData$twins$code
  length(code) > 0 && 1 %in% code
}

addChild = function(x, id, sex) {

  id = sortIds(x, id)

  if(length(id) > 2)
    stop2("Too many individuals are selected. Current selection: ", id, "<br><br>",
          "To add a child, please indicate one or both parents.")

  if(length(id) == 1) {
    if(sex == 1)
      return(addSon(x, id, verbose = FALSE))
    else
      return(addDaughter(x, id, verbose = FALSE))
  }

  if(length(id) == 2) {
    sx = getSex(x, id)
    fa = id[sx == 1]
    mo = id[sx == 2]
    if(length(fa) != 1 || length(mo) != 1)
      stop2("Incompatible sex of selected parents")

    newped = addChildren(x, father = fa, mother = mo, nch = 1, sex = sex, verbose = FALSE)
    return(newped)
  }
}

addSib = function(x, id, sex) {

  if(length(id) > 1)
    stop2("Too many individuals are selected. Current selection: ", sortIds(x, id), "<br><br>",
          "To add a sibling, please select exactly one individual.")

  if(id %in% founders(x))
    x = addParents(x, id, verbose = FALSE)

  fa = father(x, id)
  mo = mother(x, id)

  newped = addChildren(x, father = fa, mother = mo, nch = 1, sex = sex, verbose = FALSE)
  return(newped)
}


removeSel = function(currData, ids, updown) {
  newped = tryCatch(
    removeIndividuals(currData$ped, ids, remove = updown, verbose = FALSE),
    error = function(e) conditionMessage(e)
  )

  isEmpty = is.null(newped)
  discon = is.character(newped) && grepl("disconnected", newped, ignore.case = TRUE)

  errmsg = if(is.character(newped)) newped else NULL
  if(isEmpty || discon)
    errmsg = sprintf("Removing %s would leave a disconnected or empty pedigree",
                     ifelse(length(ids) == 1, paste("individual", ids), "these individuals"))
  if(!is.null(errmsg))
    stop2(errmsg)

  newID = newped$ID
  newaff  = .myintersect(currData$aff, newID)
  newcarr = .myintersect(currData$carrier, newID)
  newdec  = .myintersect(currData$deceased, newID)

  newtw = currData$twins
  newtw = newtw[newtw$id1 %in% newID & newtw$id2 %in% newID, , drop = FALSE]

  list(ped = newped, aff = newaff, carr = newcarr, dec = newdec, tw = newtw)
}

sortIds = function(x, ids) {
  intern = internalID(x, ids)
  ids[order(intern)]
}

