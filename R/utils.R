### Helper functions for the QuickPed app

.debug = function(msg)
  if(DEBUG) cat(debugCounter <<- debugCounter+1, msg, "\n")

stop2 = function (...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

`%||%` = function(x,y)
  if(is.null(x)) y else x

mylink = function(text, href, .noWS = "outside") {
  if(missing(href))
    href = text
  shiny::a(text, href = href, .noWS = .noWS, target = "_blank")
}

.myintersect = function(x, y)
  y[match(x, y, 0L)]

.mysetdiff = function(x, y)
  unique.default(x[match(x, y, 0L) == 0L])

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

checknum = function(a, var, min = 0, max = Inf) { #print("checknum")
  if(is.na(a) || a < min || a > max)
    stop2(sprintf("Invalid value of '%s'", var))
  a
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

# TODO: Delete?
.addChild = function(x, id, sex) {

  #id = sortIds(x, id)

  if(length(id) > 2)
    stop2("Too many individuals are selected. Current selection: ", id, "<br><br>",
          "To add a child, please indicate one or both parents.")

  if(length(id) == 2) {
    sx = getSex(x, id)
    fa = id[sx == 1]
    mo = id[sx == 2]
    if(length(fa) != 1 || length(mo) != 1)
      stop2("Incompatible sex of selected parents")
  }
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

  newped = addChild(x, parents(x, id), sex = sex, verbose = FALSE)
  return(newped)
}


removeSel = function(currData, ids, updown) {
  newped = tryCatch(
    removeIndividuals(currData$ped, ids, remove = updown, verbose = FALSE),
    error = function(e) conditionMessage(e)
  )

  errmsg = NULL
  if(is.character(newped))
    errmsg = newped
  else if(is.null(newped) || !is.ped(newped))
    errmsg = sprintf("Removing %s would leave a empty pedigree",
                     ifelse(length(ids) == 1, paste("individual", ids), "these individuals"))

  if(!is.null(errmsg))
    stop2(errmsg)

  newID = labels(newped)
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

iconButton = function(id, icon, w = 1, size = "m", asp = "1/1") {

  h = switch(match.arg(size), small = 30, medium = 36)
  istyle = sprintf("background-image: url('%s'); width: 100%%; aspect-ratio: %s;",
                      icon, asp)
  actionButton(inputId = id,
               label = NULL,
               icon = icon(name = NULL, class = "custom_icon", style = istyle),
               class = "icon_button",
               style = sprintf("width: calc((20%% - 4/5*2px)*%d);", w))
}


cleanPedArgs = list(
  hatched = character(0),
  aff = character(0),
  carrier = character(0),
  deceased = character(0),
  dashed = character(0),
  cols = list(border = 1, fill = NA),
  twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0))
)

cleanVec = function(x, val) {
  if(length(x) == 1 && identical(x, val))
    return()
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
  if(nrow(mz <- twins[twins$code == 1, , drop = FALSE])) {
    sx1 = getSex(newped, mz$id1)
    sx2 = getSex(newped, mz$id2)
    if(any(sx1 > 0 & sx2 > 0 & sx1 != sx2))
      stop2("Cannot change sex of one MZ twin")
  }

  newped
}
