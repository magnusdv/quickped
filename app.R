suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(pedtools)
  library(ribd)
  library(verbalisr)
  library(rdrop2)
})

Sys.setlocale(category = "LC_ALL", "C") # avoid weird deploy error


VERSION = "2.1.1"

ui = fluidPage(

  useShinyjs(),  # Set up shinyjs

  tags$head(
    tags$style(
      HTML('
        .well {padding-top: 10px; margin-bottom: 15px}
        .btn-default {margin-left: 0px; margin-right: 0px; margin-top: 2px; margin-bottom: 2px}
        .btn-file {margin-top: 0px; margin-bottom: 0px}
        .control-label {margin-bottom: 0px; padding-bottom:0px}
        .form-group {margin-bottom: 3px}
        .irs-min, .irs-max, .irs-single {display:none; line-height: 0px}
        .irs-bar, .irs-bar-edge, .irs-line  {top: 15px !important}
        .irs-handle {top: 7px !important;}
        .irs {height: 30px; margin-bottom:11px}
      ')
    )
  ),


  # Application title
  h2(id = "title-h2", "QuickPed: An Interactive Pedigree Maker"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),

  p(bold("Purpose: "),
    "This tool provides a quick way to create pedigree plots,
    and for producing ", ital("ped files"), " describing pedigrees in text format."),

  p(bold("Instructions: "),
    "Choose a suitable start pedigree and modify it by selecting members (by clicking on them in the plot) and using appropriate buttons.
    For example, to create a new child, select the parents and press ",
    ital("Son"), " or ", ital("Daughter"), ". (If just one parent is selected, a new spouse is also created.)"),

  p(bold("More information: "),
    "Further information about QuickPed can be found at the ",
    a("ped suite homepage", href = "https://magnusdv.github.io/pedsuite/articles/web_only/quickped.html", .noWS = "outside"), "."),


  fluidRow(

    # Sidebar
    column(width = 4, style = "width:450px",

      fluidRow(

        # First control column
        column(width = 6,
          wellPanel(style = "height:430px; width:210px",
            bigHeading("Quick start"),
            selectInput("startped", "Built-in pedigree", selected = "Trio",
                        choices = c("", "Trio", "Sibship (2)", "Sibship (3)", "Half sibs (1+1)", "Half sibs (2+2)",
                                    "Linear (2)", "Linear (3)", "First cousins", "Second cousins",
                                    "Half first cousins", "Half second cousins", "Ancestral (2)", "Ancestral (3)",
                                    "Double first cousins", "Quad half first cousins",
                                    "Full sib stack (2)", "Full sib stack (3)", "Half sib stack (2)", "Half sib stack (3)"),
            ),
            br(),
            fluidRow(
              column(5, hr(style = "border-top: 1px solid #000000;")),
              column(2, "or", style = "padding-left:10px; padding-top:10px; padding-bottom:17px"),
              column(5, hr(style = "border-top: 1px solid #000000;")),
            ),
            br(),
            fileInput("loadped", label = "Load a ped file", buttonLabel = icon("folder-open"),
                      accept = "text/plain", width = "100%", placeholder = NULL),

            actionButton("reset", "Reset all", class = "btn btn-danger",
                         style = "position: absolute; bottom:30px; width: 170px")
          ),

        ),

        # Second control column
        column(width = 6,
          wellPanel(style = "height:430px; width:210px",
            bigHeading("Modify"),
            midHeading("Add"),
            pedButton("addparents", "Parents"),
            fluidRow(
              pedButton("addson", "Son", side = "left"),
              pedButton("adddaughter", "Daughter", side = "right"),
            ),
            midHeading("Remove"),
            fluidRow(
              pedButton("remove", "Individuals", side = "left"),
              pedButton("clearselection", "Selection", side = "right"),
            ),
            midHeading("Switch"),
            fluidRow(
              pedButton("swapsex", "Sex", side = "left"),
              pedButton("affection", "Affected", side = "right"),
            ),
            fluidRow(
              pedButton("carrier",  "Carrier", side = "left"),
              pedButton("deceased", "Deceased", side = "right"),
            ),
            midHeading("Twins"),
            fluidRow(
              pedButton("mz", "MZ", side = "left"),
              pedButton("dz", "DZ", side = "right")
            ),

            disabled(actionButton("undo", "Undo", class = "btn btn-warning",
                                  style = "position: absolute; bottom:30px; width: 170px")),
          ),
        )
      ),
      # Relationship descriptions
      wellPanel(style = "height:210px; width:435px",
        fluidRow(
          column(width = 6, bigHeading("Relationships")),
          column(width = 3, actionButton("coeffs", "Coeffs", class = "btn btn-success",
                                         style = "width:80px")),
          column(width = 3, actionButton("describe", "Describe", class = "btn btn-success",
                                         style = "width:80px; float:right")),
        ),
        verbatimTextOutput("description", placeholder = TRUE),
        tags$head(tags$style("#description{height:145px;}"))
      )
    ),

    # Plot window
    column(width = 4, style = "margin-bottom: 15px",
           plotOutput("plot", click = "ped_click", width = "auto", height = "auto"),
    ),

    # Settings
    column(width = 4, style = "float:right; width:420px;",
      fluidRow(
        column(width = 6,
          wellPanel(
            style = "min-height:100%; width:200px",
            bigHeading("Labels"),
            fluidRow(
              column(width = 6, align = "left", style = "padding-right:5px; padding-bottom:7px;",
                     actionButton("lab123", "1, 2, 3, ..", width = "100%",  style = "background-color: lightgray;")),
              column(width = 6, align = "right", style = "padding-left:5px; padding-bottom:7px;",
                     actionButton("labGen", "I-1, I-2, ..", width = "100%",style = "background-color: lightgray;"))
            ),
            uiOutput("labels"),
            actionButton("updateLabs", "Update", width = "100%",
                        class = "btn btn-success", style = "margin-top: 10px;"),
   ),
        ),

        # Rightmost column: Settings and save
        column(width = 6,
           wellPanel(style = "height:430px; width:180px",
                     bigHeading("Plot settings"),
                     sliderInput("width", "Width", ticks = FALSE, min = 100, max = 1000, value = 430, step = 1),
                     sliderInput("height", "Height", ticks = FALSE, min = 100, max = 1000, value = 430, step = 1),
                     sliderInput("cex", "Expansion", ticks = FALSE, min = 0.5, max = 3, value = 1.6, step = 0.1),
                     sliderInput("symbolsize", "Symbol size", ticks = FALSE, min = 0.5, max = 3, value = 1, step = 0.1),
                     sliderInput("mar", "Margins", ticks = FALSE, min = 0, max = 10, value = 3, step = 0.1),
                     br(),
                     downloadButton("savePlot", "Save plot", class = "btn btn-info", style = "width: 100%;")
           ),

           # Save ped file
           wellPanel(style = "height:210px; width:180px",
                     bigHeading("Ped file"),
                     checkboxGroupInput("include", "Include", selected  = "head",
                                        c("Headers" = "head", "Family ID" = "famid", "Affection status" = "aff")),
                     downloadButton("savePed", "Save ped file", class="btn btn-info", style = "width: 100%;"),
           )
        ),
      )


    ),
  ),

  p("This is QuickPed version", VERSION, "(",
    a("changelog", href = "https://github.com/magnusdv/quickped/blob/master/NEWS.md", .noWS = "outside"), ").",
    "QuickPed is powered by the ", a("ped suite", href = "https://cran.r-project.org/package=pedsuite", .noWS = "outside"),
    " and imports ",
    a("kinship2", href = "https://cran.r-project.org/package=kinship2", .noWS = "outside"),
    " for plotting.",
    "If you find something that isn't working properly, please file a bug report at ",
    a("https://github.com/magnusdv/quickped/issues", href = "https://github.com/magnusdv/quickped/issues", .noWS = "outside"), "."),
)



server = function(input, output, session) {

  currentPedData = reactiveVal(list(ped = nuclearPed(), aff = character(0), carrier = character(0), deceased = character(0),
                                    twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0))))

  previousStack = reactiveVal(list())

  pdat = reactiveVal(NULL)
  sel = reactiveVal(character(0))

  relText = reactiveVal(NULL)


# Update pedigree ---------------------------------------------------------


  updatePedData = function(currData, ped = NULL, aff = NULL, carrier = NULL,
                           deceased = NULL, twins = NULL, clearSel = TRUE, clearInput = TRUE, clearRel = TRUE) {
    if(is.null(ped) && is.null(aff) && is.null(carrier) && is.null(deceased) && is.null(twins))
      return()

    if(is.null(ped)) ped = currData$ped
    if(is.null(aff)) aff = currData$aff
    if(is.null(carrier)) carrier = currData$carrier
    if(is.null(deceased)) deceased = currData$deceased
    if(is.null(twins)) twins = currData$twins

    newData = list(ped = ped, aff = aff, carrier = carrier, deceased = deceased, twins = twins)
    currentPedData(newData)

    # Update stack
    previousStack(c(previousStack(), list(currData)))
    enable("undo")

    # Clear stuff
    if(clearInput)
      updateSelectInput(session, "startped", selected = "")

    if(clearRel)
      relText(NULL)

    if(clearSel)
      sel(character(0))
  }


# Startped/load -----------------------------------------------------------


  observeEvent(input$startped, {
    choice = req(input$startped)

    ped = switch(choice,
                 Trio = nuclearPed(),
                 "Sibship (2)" = nuclearPed(2),
                 "Sibship (3)" = nuclearPed(3),
                 "Half sibs (1+1)" = halfSibPed(),
                 "Half sibs (2+2)" = halfSibPed(2, 2),
                 "Linear (2)" = linearPed(2),
                 "Linear (3)" = linearPed(3),
                 "Ancestral (2)" = ancestralPed(2),
                 "Ancestral (3)" = ancestralPed(3),
                 "First cousins" = cousinPed(1),
                 "Second cousins" = cousinPed(2),
                 "Half first cousins" = halfCousinPed(1),
                 "Half second cousins" = halfCousinPed(2),
                 "Double first cousins" = doubleFirstCousins(),
                 "Quad half first cousins" = quadHalfFirstCousins(),
                 "Full sib stack (2)" = fullSibMating(1),
                 "Full sib stack (3)" = fullSibMating(2),
                 "Half sib stack (2)" = halfSibStack(2),
                 "Half sib stack (3)" = halfSibStack(3),
                 errModal("Sorry, this pedigree is not implemented yet.")
    )

    currData = currentPedData()

    updatePedData(currData, ped = req(ped), aff = character(0), carrier = character(0), deceased = character(0),
                  twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0)),
                  clearInput = FALSE)
  })


  observeEvent(input$loadped, {
    file = req(input$loadped$datapath)
    cls = c("id", "fid", "mid", "sex")

    ped = tryCatch(
      expr = {
        df = read.table(file, header = TRUE, sep = "\t", colClasses = "character",
                        check.names = FALSE)
        names(df) = nms = tolower(names(df))
        if(!all(cls %in% nms))
          stop("Column not found: ", toString(setdiff(cls, nms)))

        as.ped(df[cls])
      },
      error = function(e) errModal(conditionMessage(e)),
      warning = function(e) errModal(conditionMessage(e))
    )

    req(ped)

    # Affected
    aff = if("aff" %in% nms) ped$ID[df$aff == 2] else character(0)

    currData = currentPedData()

    updatePedData(currData, ped = req(ped), aff = aff, carrier = character(0), deceased = character(0),
                  twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0)))
  })


# Modify pedigree ---------------------------------------------------------


  observeEvent(input$addson, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addChild(currData$ped, id, sex = 1),
                      error = function(e) errModal(e))
    updatePedData(currData, ped = newped, clearSel = FALSE)
  })

  observeEvent(input$adddaughter, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addChild(currData$ped, id, sex = 2),
                      error = function(e) errModal(e))
    updatePedData(currData, ped = newped, clearSel = FALSE)
  })

  observeEvent(input$addparents, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addParents(currData$ped, id, verbose = FALSE),
             error = function(e) errModal(e))
    updatePedData(currData, ped = newped)
  })

  observeEvent(input$swapsex, {
    currData = currentPedData()
    id = sel()
    newped = swapSex(currData$ped, id, verbose = FALSE)
    updatePedData(currData, ped = newped, clearSel = length(id) > 1)
  })

  observeEvent(input$affection, {
    id = req(sel())
    currData = currentPedData()
    aff = currData$aff
    newAff = setdiff(union(aff, id), intersect(aff, id))
    updatePedData(currData, aff = newAff, clearRel = FALSE)

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
    updatePedData(currData, carrier = newCarr, clearSel = length(id) > 1, clearRel = FALSE)
  })

  observeEvent(input$deceased, {
    id = req(sel())
    currData = currentPedData()
    deceased = currData$deceased
    newDec = setdiff(union(deceased, id), intersect(deceased, id))
    updatePedData(currData, deceased = newDec, clearSel = length(id) > 1, clearRel = FALSE)
  })

  observeEvent(input$remove, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(
      removeIndividuals(currData$ped, id, verbose = FALSE),
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
                  deceased = newdec, twins = newtw)
  })

  observeEvent(input$clearselection, sel(character(0)))

  observeEvent(input$mz, {
    ids = req(sel())
    currData = currentPedData()
    ped = currData$ped

    # Checks
    err = NULL

    if(length(ids) != 2)
      err = "To change twin status, please select exactly 2 individuals."
    else if(all(ids %in% founders(ped)))
      err = "Founders cannot be twins"
    else if(!identical(parents(ped, ids[1]), parents(ped, ids[2])))
      err = "Twins have the same parents"
    else if(getSex(ped, ids[1]) != getSex(ped, ids[2]))
      err = "MZ twins must have the same sex"

    if(!is.null(err)) {
      errModal(err)
      return()
    }

    twins = updateTwins(currData$twins, ids, code = 1L)
    updatePedData(currData, twins = twins)
  })

  observeEvent(input$dz, {
    ids = req(sel())
    currData = currentPedData()
    ped = currData$ped

    # Checks
    err = NULL

    if(length(ids) != 2)
      err = "To change twin status, please select exactly 2 individuals."
    else if(all(ids %in% founders(ped)))
      err = "Founders cannot be twins"
    else if(!identical(parents(ped, ids[1]), parents(ped, ids[2])))
      err = "Twins must have the same parents"

    if(!is.null(err)) {
      errModal(err)
      return()
    }

    twins = updateTwins(currData$twins, ids, code = 2L)
    updatePedData(currData, twins = twins)
  })



# Undo/reset --------------------------------------------------------------


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

    # Clear stuff
    relText(NULL)
    updateSelectInput(session, "startped", selected = "")
  })

  observeEvent(input$reset, {
    if(input$startped == "Trio")
      updateSelectInput(session, "startped", selected = "")
    updateSelectInput(session, "startped", selected = "Trio")

    # Reset plot settings
    updateSliderInput(session, "width", value = 430)
    updateSliderInput(session, "height", value = 430)
    updateSliderInput(session, "cex", value = 1.6)
    updateSliderInput(session, "symbolsize", value = 1)
    updateSliderInput(session, "mar", value = 3)
  })



# Labels ------------------------------------------------------------------

  output$labels = renderUI({
    labs = labels(currentPedData()$ped)
    fields = paste0("lab", seq_along(labs))
    lapply(seq_along(labs), function(i)
      textInput2(fields[i], value = labs[i])) #textInput(fields[i], label = NULL, value = labs[i], width = "100%"))
  })

  observeEvent(input$lab123, {
    currData = currentPedData()
    ped = currData$ped

    # Current labels in plot order
    old = getPlotOrder(ped = ped, plist = pdat()$plist)

    # New labels: 1, 2, ...
    new = seq_along(old)

    newData = updateLabelsData(currData, old = old, new = new, reorder = TRUE)

    do.call(updatePedData, c(list(currData = currData), newData))
  })

  observeEvent(input$labGen, {
    currData = currentPedData()
    ped = currData$ped

    # Current labels in plot order
    oldList = getPlotOrder(ped = ped, plist = pdat()$plist, perGeneration = TRUE)
    old = unlist(oldList)

    # New labels: I-1, ...
    gen = rep(seq_along(oldList), lengths(oldList))
    idx = unlist(lapply(lengths(oldList), seq_len))
    new = paste(as.roman(gen), idx, sep = "-")

    newData = updateLabelsData(currData, old = old, new = new, reorder = TRUE)

    do.call(updatePedData, c(list(currData = currData), newData))
  })

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
      errModal("Duplicated ID label: ", newlabs[dup])
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

    newData = updateLabelsData(currData, old = oldlabs, new = newlabs)

    do.call(updatePedData, c(list(currData = currData), newData))
  })


  # Plot --------------------------------------------------------------------

  plotArgs = reactive({
    m = input$mar
    adjmar = c(max(m - 1, 0), m, m + 1, m)
    list(cex = input$cex, symbolsize = input$symbolsize, mar = adjmar)
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



  # Save --------------------------------------------------------------------


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


# Relationships ------------------------------------------------

  observeEvent(input$describe, {
    ids = req(sel())
    if(length(ids) != 2) {
      errModal("Please select exactly two individuals. Current selection: ", ids)
      return()
    }

    ped = currentPedData()$ped

    txt = verbalisr::verbalise(ped, ids, verbose = FALSE)
    relText(toupper(txt))
  })

  observeEvent(input$coeffs, {
    ids = sort(req(sel()))
    N = length(ids)
    ped = currentPedData()$ped

    # Inbreeding
    inb = ribd::inbreeding(ped, ids)
    txt = c("Inbreeding coefficients:", sprintf("* %s: f = %g", ids, inb))

    if(N != 2) {
      txt = c(txt, "", "For pairwise coefficients, select 2 individuals.")
      relText(txt)
      return()
    }

    ### Pairwise coefficients
    txt =  c(txt, "", sprintf("Pairwise coefficients between %s and %s:", ids[1], ids[2]))

    # Kinship
    phi = ribd::kinship(ped, ids)
    txt = c(txt, paste0("* Kinship: phi = ", phi))

    # Kappa (if both outbred) or Delta
    if(all(inb == 0)) {
      kap = ribd::kappaIBD(ped, ids)
      txt = c(txt, paste0("* Kappa = (", toString(kap), ")"))
    }
    else {
      kap = c(NA, NA, NA)
      delta = ribd::condensedIdentity(ped, ids)
      txt = c(txt, paste0("* Kappa = (", toString(kap), ")"),
              "* Condensed identity:", sprintf("    Delta%d = %g", 1:9, delta))
    }

    relText(txt)
  })

  output$description = renderText(req(relText()), sep = "\n")

}


shinyApp(ui = ui, server = server)
