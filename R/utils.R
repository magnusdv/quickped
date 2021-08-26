### Helper functions for the QuickPed app

stop2 = function (...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

bigHeading = function(x)
  h4(strong(x), .noWS = "before")

midHeading = function(x)
  h5(strong(x), style = "margin-bottom: 0px;")

bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")

errModal = function(..., html = FALSE) {
  mess = paste(lapply(list(...), toString), collapse = "")
  if(html)
    mess = HTML(mess)
  showModal(modalDialog(mess))
}


textInput2 = function(inputId, value) {
  w  = textInput(inputId, label = NULL, value = value, width = "100%")
  w$children[[2]]$attribs[["style"]] = "padding-top, padding-bottom: 1px; height: 28px;"
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


addChild = function(x, id, sex) {

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
    if(length(fa) != 1 || length(mo) != 1) {
      errModal("Incompatible sex of selected parents")
      return()
    }
    newped = addChildren(x, father = fa, mother = mo, nch = 1, sex = sex, verbose = FALSE)
    return(newped)
  }

  if(length(id) > 2) {
    errModal("Please select either 1 or 2 individuals. Current selection: ", id)
    return()
  }

}

pdat2df = function(pdat) {
  n = pdat$plist$n
  nid = pdat$plist$nid
  pos = pdat$plist$pos

  # Coordinates of top point of each symbol
  x = unlist(lapply(seq_along(n), function(i) pos[i, 1:n[i]]))
  y = rep(seq_along(n), n)

  # Adjust y to give symbol centre
  y = y + pdat$boxh/2

  # Internal ID
  idInt = unlist(lapply(seq_along(n), function(i) nid[i, 1:n[i]]))

  data.frame(x = x, y = y, idInt = idInt)
}


# Dropbox
token = readRDS("droptoken.rds")

dropup = function(obj) {
  tmppath = file.path(tempdir(), sprintf("%s.rds", format(Sys.time(), format = "%Y-%m-%d_%H.%M.%S")))
  saveRDS(obj, tmppath)
  suppressMessages(drop_upload(tmppath, path = "quickped", mode = "add", verbose = FALSE, dtoken = token))
}

sortIds = function(x, ids) {
  intern = internalID(x, ids)
  ids[order(intern)]
}
