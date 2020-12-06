### Helper functions for the pedshape app

errModal = function(mess) {
  showModal(modalDialog(mess))
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
    errModal("Too many parents selected")
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

