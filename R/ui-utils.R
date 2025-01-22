bigHeading = function(x)
  h4(strong(x), .noWS = "before")

midHeading = function(x, style = NULL)
  h5(strong(x), style = paste("margin-bottom: 0px;", style))

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

textInput2 = function(inputId, value, height) {
  w  = textInput(inputId, label = NULL, value = value, width = "100%")
  w$children[[2]]$attribs[["style"]] = sprintf("padding-top: 1px; padding-bottom: 1px; height: %dpx;", height)
  w
}

pedButton = function(id, label, side = NULL, ...) {
  sty = "padding-top: 5px; padding-bottom: 5px; padding-left: 0px; padding-right: 0px"
  but = actionButton(id, label, width = "100%", style = sty, ...)
  if(is.null(side))
    return(but)

  switch(side,
   left = column(6, align = "left", style = "padding-right: 3px;", but),
   right = column(6, align = "right", style = "padding-left: 3px;", but)
  )
}

iconButton = function(id, icon, float = "left") {
  istyle = sprintf("background-image: url('%s'); width: 100%%; aspect-ratio: 1/1;", icon)

  actionButton(inputId = id,
               label = NULL,
               icon = icon(name = NULL, class = "custom_icon", style = istyle),
               class = "icon_button",
               style = sprintf("width: calc(20%% - 4/5*2px); float: %s;", float))
}

parentMessage = modalDialog(
  title = "Parent Assignment Guidance",
  HTML("When 2 or 3 individuals are selected, adding parents works by
    considering the order in which the individuals were selected.
    The first is interpreted as the child, followed by its designated parent(s).<br><br>
    If this was not the intended action, simply press <b>Undo</b> after closing this message."),
  footer = tagList(
    div(style = "display: inline-block; vertical-align: middle;",
        checkboxInput("suppressParentMessage", "Don't show this message again")),
    div(style = "display: inline-block; vertical-align: middle; margin-left: 20px;",
        modalButton("OK"))
  )
)
