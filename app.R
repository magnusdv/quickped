suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(pedtools)
  library(rdrop2)
})

Sys.setlocale(category = "LC_ALL", "C") # avoid weird deploy error
VERSION = "1.2.0"

ui = fluidPage(

  useShinyjs(),  # Set up shinyjs

  tags$head(
    tags$style(
      HTML('
        .well {padding-top: 10px}
        .btn-default {margin: 2px;}
        .form-group {margin-bottom: 3px;}
        .form-control {padding-top, padding-bottom: 1px; height: 28px;}
        .control-label {margin-bottom: 0px; padding-bottom:0px}
        .irs-min, .irs-max, .irs-single {display:none; line-height: 0px}
        .irs-bar, .irs-bar-edge, .irs-line  {top: 12px}
        .irs-slider {top: 4px;}
        .irs {height: 30px; margin-bottom:11px}
      ')
    )
  ),


  # Application title
  h2(id = "title-h2", "QuickPed: An Interactive Pedigree Creator"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),

  p(strong("Purpose: ", .noWS = "outside"),
    "This tool provides a quick way to create pedigree plots,
    and for producing ", em("ped files", .noWS = "outside"),
    " describing pedigrees in text format.
    Such files are often required as input for pedigree analysis software."),

  p(strong("Instructions: ", .noWS = "outside"),
    "Construct the pedigree by selecting pedigree members (by clicking on them in the plot) and using appropriate buttons.
    For example, new children can be created by selecting one or two individuals and pressing either ",
    em("Son", .noWS = "outside"), " or ", em("Daughter", .noWS = "outside"),
    ". If needed, replace the default labels by other names, and use the ",
    em("Sex", .noWS = "outside"),
    " button to switch sex of selected individuals."),

  p(strong("More information: ", .noWS = "outside"),
    "Further explanations, source code and bug reports can be found at ",
    a("GitHub", href = "https://github.com/magnusdv/quickped", .noWS = "outside"), "."),

  #br(),

  fluidRow(

    # Sidebar
    column(width = 6, style = "width:500px",

      fluidRow(

        # First control column
        column(width = 6,
          wellPanel(style = "height:430px",
            h4(strong("Plot settings"), .noWS = "before"),
            sliderInput("width", "Width", ticks = FALSE, min = 100, max = 1000, value = 430, step = 1),
            sliderInput("height", "Height", ticks = FALSE, min = 100, max = 1000, value = 430, step = 1),
            sliderInput("cex", "Expansion", ticks = FALSE, min = 0.5, max = 3, value = 1.6, step = 0.1),
            sliderInput("symbolsize", "Symbol size", ticks = FALSE, min = 0.5, max = 3, value = 1, step = 0.1),
            sliderInput("mar", "Margins", ticks = FALSE, min = 0, max = 10, value = 3, step = 0.1),
            br(),
            downloadButton("savePlot", "Save plot", class = "btn btn-info", style = "width: 100%; margin-left:0px; margin-right:0px")
          ),

          wellPanel(
            fluidRow(
              column(width = 6, h4(strong("Labels"), .noWS = "before")),
              column(width = 6, actionButton("lab123", "1-2-3", width = "100%",
                                             style = "background-color: lightgray; margin-left:0px; margin-right:0px"))
            ),
            uiOutput("labels"),
            actionButton("updateLabs", "Update", width = "100%",
                         class = "btn btn-success", style = "margin-top: 10px; margin-left:0px; margin-right:0px"),
          )
        ),

        # Second control column
        column(width = 6,
          wellPanel(style = "height:430px",
            h4(strong("Build pedigree"), .noWS = "before"),
            h5(strong("Add"), style = "margin-bottom: 0px;"),
            pedButton("addparents", "Parents"),
            fluidRow(
              pedButton("addson", "Son", side = "left"),
              pedButton("adddaughter", "Daughter", side = "right"),
            ),
            h5(strong("Remove"), style = "margin-bottom: 0px;"),
            pedButton("remove", "Remove selected"),
            h5(strong("Switch"), style = "margin-bottom: 0px;"),
            fluidRow(
              pedButton("swapsex", "Sex", side = "left"),
              pedButton("affection", "Affected", side = "right"),
            ),
            fluidRow(
              pedButton("carrier",  "Carrier", side = "left"),
              pedButton("deceased", "Deceased", side = "right"),
            ),
            h5(strong("Twins"), style = "margin-bottom: 0px;"),
            fluidRow(
              pedButton("mz", "MZ", side = "left"),
              pedButton("dz", "DZ", side = "right")
            ),

            div(style="margin-bottom: 20px"),
            fluidRow(
              column(6, align = "left", style = "padding-right: 4px;",
                     disabled(actionButton("undo", "Undo", width = "100%", class = "btn btn-warning"))),
              column(6, align = "right", style = "padding-left: 4px;",
                     actionButton("reset", "Reset", width = "100%", class = "btn btn-danger")),
            )
          ),
          wellPanel(
            h4(strong("Ped file")),
            checkboxGroupInput("include", "Include", selected  = "head",
                               c("Headers" = "head", "Family ID" = "famid", "Affection status" = "aff")),
            downloadButton("savePed", "Save ped file", class="btn btn-info",
                           style = "width: 100%; margin-left:0px; margin-right:0px"),
          )
        )
      )
    ),

    # Plot window
    mainPanel(width = 6, plotOutput("plot", click = "ped_click", width = "auto"),
    )
  ),

  p("This is QuickPed version", VERSION, "(",
    a("changelog", href = "https://github.com/magnusdv/quickped/blob/master/NEWS.md", .noWS = "outside"), ").",
    "If you find something that isn't working properly, don't hesitate to file a bug report at ",
    a("https://github.com/magnusdv/quickped/issues", href = "https://github.com/magnusdv/quickped/issues", .noWS = "outside"), "."),
)



server = function(input, output, session) {

  currentPedData = reactiveVal(list(ped = nuclearPed(), aff = character(0), carrier = character(0), deceased = character(0),
                                    twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0))))

  previousStack = reactiveVal(list())

  pdat = reactiveVal(NULL)
  sel = reactiveVal(character(0))

  updatePedData = function(currData, ped = NULL, aff = NULL, carrier = NULL,
                           deceased = NULL, twins = NULL, emptySel = FALSE) {
    if(is.null(ped) && is.null(aff) && is.null(carrier) && is.null(deceased) && is.null(twins))
      return()

    if(is.null(ped)) ped = currData$ped
    if(is.null(aff)) aff = currData$aff
    if(is.null(carrier)) carrier = currData$carrier
    if(is.null(deceased)) deceased = currData$deceased
    if(is.null(twins)) twins = currData$twins

    newData = list(ped = ped, aff = aff, carrier = carrier, deceased = deceased, twins = twins)
    currentPedData(newData)
    previousStack(c(previousStack(), list(currData)))
    enable("undo")

    if(emptySel)
      sel(character(0))
  }

  plotArgs = reactive({
    m = input$mar
    adjmar = c(max(m - 1, 0), m, m + 1, m)
    list(cex = input$cex, symbolsize = input$symbolsize, mar = adjmar)
  })

  output$labels = renderUI({
    labs = labels(currentPedData()$ped)
    fields = paste0("lab", seq_along(labs))
    lapply(seq_along(labs), function(i)
      textInput(fields[i], label = NULL, value = labs[i], width = "100%"))
  })

  observeEvent(input$lab123, {
    currData = currentPedData()
    ped = currData$ped

    # Relabel according to current plot
    p = pdat()$plist
    plotInt = unlist(lapply(seq_along(p$n), function(i) p$nid[i, 1:p$n[i]]))
    if (anyDuplicated(plotInt))
      plotInt = unique.default(plotInt)
    plotlabs = labels(ped)[plotInt] # individuals in plot order

    newped = relabel(ped, old = plotlabs, new = seq_along(plotlabs), reorder = TRUE)
    newaff = match(currData$aff, plotlabs)
    newcarr = match(currData$carrier, plotlabs)
    newdec = match(currData$deceased, plotlabs)

    newtw = currData$twins
    newtw$id1 = match(newtw$id1, plotlabs)
    newtw$id2 = match(newtw$id2, plotlabs)

    updatePedData(currData, ped = newped, aff = newaff, carrier = newcarr,
                  deceased = newdec, twins = newtw, emptySel = TRUE)
  })


  #observeEvent(input$loadped, {
  #  tryCatch(
  #    read.table("quickped.ped"),
  #    error = function(e) {errModal(conditionMessage(e)); return()}
  #  )
  #  updatePedData(currData, ped = newped, aff = newaff, emptySel = TRUE)
  #})


  observeEvent(input$updateLabs, {
    currData = currentPedData()
    ped = currData$ped
    oldlabs = labels(ped)
    fields = paste0("lab", seq_along(oldlabs))
    newlabs = as.character(vapply(fields, function(s) input[[s]], FUN.VALUE = "1"))
    newlabs = trimws(newlabs)

    if(identical(newlabs, oldlabs))
      return()
    if(dup <- anyDuplicated(newlabs)) {
      errModal(paste("Duplicated ID label:", newlabs[dup]))
      return()
    }
    if(0 %in% newlabs) {
      errModal('"0" cannot be used as label')
      return()
    }
    if("" %in% newlabs) {
      errModal("Empty label")
      return()
    }
    newped = relabel(ped, new = newlabs)
    newaff = newlabs[internalID(ped, currData$aff)]
    newcarr = newlabs[internalID(ped, currData$carrier)]
    newdec = newlabs[internalID(ped, currData$deceased)]

    newtw = currData$twins
    newtw$id1 = newlabs[internalID(ped, newtw$id1)]
    newtw$id2 = newlabs[internalID(ped, newtw$id2)]

    updatePedData(currData, ped = newped, aff = newaff, carrier = newcarr,
                  deceased = newdec, twins = newtw, emptySel = TRUE)
  })

  output$plot = renderPlot({
    currData = currentPedData()
    ped = currData$ped
    args = plotArgs()
    selected = sel()

    dat = tryCatch(
      plot(ped, aff = currData$aff, carrier = currData$carrier,
           deceased = currData$deceased, twins = currData$twins,
           col = list(red = selected), cex = args$cex,
           symbolsize = args$symbolsize, margins = args$mar),
      error = function(e) {
        msg = conditionMessage(e)
        if(grepl("reduce cex", msg))
          msg = "Plot region is too small"
        errModal(msg)
        return()
      })

    box("outer", col = 1)

    if(!is.null(dat))
      pdat(dat)
  }, execOnResize = TRUE, width = function() input$width, height = function() input$height)


  output$savePed = downloadHandler(
    filename = "quickped.ped",
    content = function(con) {
      inclHead = "head" %in% input$include
      inclFamid = "famid" %in% input$include
      inclAff = "aff" %in% input$include

      currData = currentPedData()
      ped = currData$ped
      df = as.data.frame(ped)
      if(inclFamid)
        df = cbind(famid = 1, df)
      if(inclAff)
        df = cbind(df, aff = ifelse(labels(ped) %in% currData$aff, 2, 1))

      write.table(df, file = con, col.names = inclHead, row.names = FALSE,
                  quote = FALSE, sep = "\t")
      dropup(df)
    }
  )

  output$savePlot = downloadHandler(
    filename = "quickped.png",
    content = function(con) {
      currData = currentPedData()
      args = plotArgs()
      png(con, width = input$width, height = input$height)
      plot(currData$ped, aff = currData$aff, carrier = currData$carrier,
           deceased = currData$deceased, twins = currData$twins,
           cex = args$cex, symbolsize = args$symbolsize, margins = args$mar)
      dev.off()
      dropup(list(currendPedData = currData, plotArgs = args))
    },
    contentType = "image/png"
  )

  observeEvent(input$ped_click, {
    posDf = pdat2df(pdat())

    idInt = nearPoints(posDf, input$ped_click, xvar = "x", yvar = "y",
                    threshold = 20, maxpoints = 1)$idInt
    if(length(idInt) == 0)
      return()

    currData = currentPedData()
    id = labels(currData$ped)[idInt]

    currSel = sel()
    if(id %in% currSel)
      sel(setdiff(currSel, id))
    else
      sel(c(currSel, id))
  })

  observeEvent(input$addson, {
    id = req(sel())
    currData = currentPedData()
    newped = addChild(currData$ped, id, sex = 1)
    updatePedData(currData, ped = newped)
  })

  observeEvent(input$adddaughter, {
    id = req(sel())
    currData = currentPedData()
    newped = addChild(currData$ped, id, sex = 2)
    updatePedData(currData, ped = newped)
  })

  observeEvent(input$addparents, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addParents(currData$ped, id, verbose = FALSE),
             error = function(e) errModal(e))
    updatePedData(currData, ped = newped, emptySel = TRUE)
  })

  observeEvent(input$swapsex, {
    currData = currentPedData()
    id = sel()
    newped = swapSex(currData$ped, id, verbose = FALSE)
    updatePedData(currData, ped = newped, emptySel = length(id) > 1)
  })

  observeEvent(input$affection, {
    id = req(sel())
    currData = currentPedData()
    aff = currData$aff
    newAff = setdiff(union(aff, id), intersect(aff, id))
    updatePedData(currData, aff = newAff, emptySel = length(id) > 1)

    # Update checkbox "Include affection status"
    inc = input$include
    newInc = if(length(newAff) == 0) setdiff(inc, "aff") else union(inc, "aff")
    updateCheckboxGroupInput(session, "include", selected = newInc)
  })

  observeEvent(input$carrier, {
    id = req(sel())
    currData = currentPedData()
    carrier = currData$carrier
    newCarr = setdiff(union(carrier, id), intersect(carrier, id))
    updatePedData(currData, carrier = newCarr, emptySel = length(id) > 1)
  })

  observeEvent(input$deceased, {
    id = req(sel())
    currData = currentPedData()
    deceased = currData$deceased
    newDec = setdiff(union(deceased, id), intersect(deceased, id))
    updatePedData(currData, deceased = newDec, emptySel = length(id) > 1)
  })

  observeEvent(input$remove, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch({removeIndividuals(currData$ped, id, verbose = FALSE)},
      error = function(e) {
        msg = conditionMessage(e)
        if(!grepl("Disconnected", msg, ignore.case = TRUE)) # if disconnected, errModal later
          errModal(msg)
        return()
      })

    if(is.null(newped)) {
      errModal(sprintf("Removing %s would disconnect the pedigree",
                       ifelse(length(id) == 1, paste("individual", id), "these individuals")))
      return()
    }
    newaff = setdiff(currData$aff, id)
    newcarr = setdiff(currData$carrier, id)
    newdec = setdiff(currData$deceased, id)

    newtw = currData$twins
    newtw = newtw[newtw$id1 != id & newtw$id2 != id, , drop = FALSE]

    updatePedData(currData, ped = newped, aff = newaff, carrier = newcarr,
                  deceased = newdec, twins = newtw, emptySel = TRUE)
  })

  observeEvent(input$mz, {
    ids = req(sel())
    if(length(ids) != 2) {
      errModal("To change twin status, please select exactly 2 individuals")
      return()
    }
    currData = currentPedData()
    twins = updateTwins(currData$twins, ids, code = 1L)
    updatePedData(currData, twins = twins, emptySel = TRUE)
  })

  observeEvent(input$dz, {
    ids = req(sel())
    if(length(ids) != 2) {
      errModal("To change twin status, please select exactly 2 individuals")
      return()
    }
    currData = currentPedData()
    twins = updateTwins(currData$twins, ids, code = 2L)
    updatePedData(currData, twins = twins, emptySel = TRUE)
  })

  observeEvent(input$undo, {
    stack = previousStack()
    len = length(stack)
    if(len == 0)
      return()
    currentPedData(stack[[len]])
    previousStack(stack[-len])

    # Remove invalid IDs from selection
    sel(intersect(sel(), labels(currentPedData()$ped)))

    if(len == 1)
      disable("undo")
  })

  observeEvent(input$reset, {
    currData = currentPedData()
    updatePedData(currData,
                  ped = nuclearPed(), aff = character(0), carrier = character(0), deceased = character(0),
                  twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0)),
                  emptySel = TRUE)
  })


}


shinyApp(ui = ui, server = server)
