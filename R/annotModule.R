txtAlign = c(top = "center", topright = "left", right = "left",
             bottomright = "left", bottom = "center", bottomleft = "right",
             left = "right", topleft = "right", inside = "center")

ns = NS("textAnnot")

# Custom stripped-down textInput function with text alignment
txtInp = function(pos, id, ann, width = "100px") {
  currVec = ann[[pos]]
  val = if(id %in% names(currVec)) currVec[id] else ""

  tags$input(
    id = ns(pos),
    type = "text",
    class = "shiny-input-text form-control",
    style = sprintf("text-align: %s; font-weight: bold; width: %s; margin: auto; padding: 5px",
                    txtAlign[[pos]], width),
    value = val,
    placeholder = pos
  )
}


# Function to create and handle the annotation modal
showAnnotationModal = function(input, output, session, id, annot) {  .debug("open-textAnnot")

  currAnn = annot()

  showModal(modalDialog(
    title = paste("Text annotation for individual", id),
    tags$style(type = 'text/css', "
      #grid-container {
        display: grid;
        gap: 3px;
        justify-content: center;
        align-items: center;
        margin-top: 20px;
      }
      #symbol {
        grid-column: 2;
        grid-row: 2;
        border: none;
        background: lightgray;
        width: 200px;
        height: 200px;
        display: flex;
      }
    "),
    div(id = 'grid-container',
      div(txtInp("topleft", id, ann = currAnn),  style = "grid-area: 1 / 1 / 2 / 2;"),
      div(txtInp("top", id, ann = currAnn), style = "grid-area: 1 / 2 / 2 / 3;"),
      div(txtInp("topright", id, ann = currAnn), style = "grid-area: 1 / 3 / 2 / 4;"),
      div(txtInp("left", id, ann = currAnn), style = "grid-area: 2 / 1 / 3 / 2;"),
      div(id = "symbol", txtInp("inside", id, ann = currAnn)),
      div(txtInp("right", id, ann = currAnn), style = "grid-area: 2 / 3 / 3 / 4;"),
      div(txtInp("bottomleft", id, ann = currAnn), style = "grid-area: 3 / 1 / 4 / 2;"),
      div(txtInp("bottom", id, ann = currAnn), style = "grid-area: 3 / 2 / 4 / 3;"),
      div(txtInp("bottomright", id, ann = currAnn), style = "grid-area: 3 / 3 / 4 / 4;")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save"), "Save", class = "btn btn-primary")
    ),
    easyClose = TRUE
  ))

  observeEvent(input[[ns("save")]], {  .debug("save-textAnnot")
    ann = annot()
    for(p in names(txtAlign)) {
      oldvec = ann[[p]] %||% character(0)
      oldvec[id] = input[[ns(p)]]
      ann[[p]] = oldvec[nzchar(oldvec)]
    }
    ann = ann[lengths(ann) > 0]
    annot(ann)
    removeModal()
  }, ignoreInit = TRUE, once = TRUE)
}
