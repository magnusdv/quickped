suppressPackageStartupMessages({
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(pedtools)
  library(ribd)
  library(verbalisr)
  library(ggplot2)
  library(ggrepel)
})


VERSION = "3.2.0"

ui = fluidPage(

  includeCSS("www/custom.css"),
  tags$head(includeHTML("GA.html")),
  useShinyjs(),

  # Application title
  h2(id = "title-h2", "QuickPed: An Interactive Pedigree Creator"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),

  p(bold("Purpose: "), style = "margin: 0 0 5px",
    "QuickPed lets you rapidly create attractive pedigree plots, save them as images or text files, and analyse the relationships within them."),

  p(bold("Instructions: "), style = "margin: 0 0 5px",
    "Choose a suitable start pedigree and modify it by clicking on individuals and using appropriate buttons.
    For example, to create a new child, select the parents and press ", ital("Son"), " or ", ital("Daughter"), ".",
    "Check out the ",
    mylink("online user manual", "https://magnusdv.github.io/pedsuite/articles/web_only/quickped.html"),
    " for various tips and tricks, including an introduction to relatedness coefficients."),

  p(bold("Citation: "), style = "margin: 0 0 10px",
    "If you use QuickPed in a publication, please cite this paper: ",
    "Vigeland MD (2022). QuickPed: an online tool for drawing pedigrees and analysing relatedness. ",
    ital("BMC Bioinformatics"), ", ", bold("23"), ". DOI:",
    mylink("10.1186/s12859-022-04759-y", "https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-022-04759-y"),
    "."),


  fluidRow(

    # Sidebar
    column(width = 4, style = "width:450px",

      fluidRow(

        # First control column
        column(width = 6,
          wellPanel(style = "height:430px; width:210px",
            bigHeading("Quick start"),
            selectInput("startped", "Built-in pedigree", selected = "Trio", choices = BUILTIN_CHOICES, width = "100%"),
            br(),
            fluidRow(
              column(5, hr(style = "border-top: 1px solid #000000; margin-top: 7px; margin-bottom: 12px")),
              column(2, "or", style = "padding-left:10px;"),
              column(5, hr(style = "border-top: 1px solid #000000; margin-top: 7px; margin-bottom: 12px")),
            ),
            br(),
            fileInput("loadped", label = "Load a ped file", buttonLabel = icon("folder-open"),
                      accept = c(".ped", ".txt", ".csv", ".tsv"),
                      width = "100%", placeholder = NULL),
            fluidRow(
              column(5, hr(style = "border-top: 1px solid #000000; margin-top: 15px; margin-bottom: 20px")),
              column(2, "or", style = "padding-left:10px; margin-top: 5px"),
              column(5, hr(style = "border-top: 1px solid #000000; margin-top: 15px; margin-bottom: 20px")),
            ),
            br(),
            pedButton("randomped", "Random pedigree"),
            br(),
            actionButton("reset", "Reset all", class = "btn btn-danger",
                         style = "position: absolute; bottom:30px; width: 170px")
          ),

        ),

        # Second control column
        column(width = 6,
          wellPanel(style = "height:430px; width:210px",
            bigHeading("Modify"),
            midHeading("Add"),
            fluidRow(
              pedButton("addson", "Son", side = "left"),
              pedButton("adddaughter", "Daughter", side = "right"),
            ),
            fluidRow(
              pedButton("addsibling", "Sibling", side = "left"),
              pedButton("addparents", "Parents", side = "right"),
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
            midHeading("Remove"),
            fluidRow(
              column(6, align = "left", style = "padding-right: 3px;",
                fluidRow(
                  column(6, style = "padding-right: 3px;",
                         actionButton("removeDown", icon("arrow-down"), width = "100%",
                                      style = "padding-top: 5px; padding-bottom: 5px; padding-left: 0px; padding-right: 0px")),
                  column(6, style = "padding-left: 3px;",
                         actionButton("removeUp", icon("arrow-up"), width = "100%",
                                      style = "padding-top: 5px; padding-bottom: 5px; padding-left: 0px; padding-right: 0px")),
               )),
              pedButton("clearselection", "Deselect", side = "right"),
              bsTooltip("removeDown", HTML("Remove&nbsp;selected + descendants"), placement = "top"),
              bsTooltip("removeUp", HTML("Remove&nbsp;selected + ancestors"), placement = "top"),
              bsTooltip("clearselection", "Deselect all", placement = "top"),
            ),
            disabled(actionButton("undo", "Undo", class = "btn btn-warning",
                                  style = "position: absolute; bottom:30px; width: 170px")),
          ),
        )
      ),
      # Relationship descriptions
      wellPanel(style = "height:210px; width:435px",
        fluidRow(
          column(width = 4, bigHeading("Relationships")),
          column(width = 2, actionButton("describe", icon("comment"), class = "btn btn-success", onclick = "buttonClick('describe')")),
          column(width = 2, actionButton("coeffs", icon("calculator"), class = "btn btn-success"), onclick = "buttonClick('coeffs')"),
          column(width = 2, actionButton("triangle", icon("chart-area"), class = "btn btn-success"), onclick = "buttonClick('triangle')"),
          column(width = 2, actionButton("coeffTable", icon("list"), class = "btn btn-success"), onclick = "buttonClick('coeffTable')"),
        ),
        bsTooltip("describe", "Describe relationship", placement = "top"),
        bsTooltip("coeffs", "Calculate coefficients", placement = "top"),
        bsTooltip("triangle", "Plot kappa coefficients", placement = "top"),
        bsTooltip("coeffTable", "Table of coefficients", placement = "top"),
        verbatimTextOutput("description", placeholder = TRUE),
        tags$head(tags$style("#description{height:145px;padding-bottom:2px;}"))
      )
    ),

    # Plot window
    column(width = 4, style = "margin-bottom: 15px",
           plotOutput("plot", click = "ped_click",  dblclick = "ped_dblclick", width = "auto", height = "auto"),
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
                     actionButton("labs123", "1, 2, 3, ..", width = "100%",  style = "background-color: lightgray;")),
              column(width = 6, align = "right", style = "padding-left:5px; padding-bottom:7px;",
                     actionButton("labsGen", "I-1, I-2, ..", width = "100%",style = "background-color: lightgray;"))
            ),
            radioButtons("showlabs", label = NULL, choices = c("Show all" = "show", "Hide all" = "hide"),
                         inline = TRUE, selected = "show", width = "100%"),
            uiOutput("labels"),
            actionButton("updateLabs", "Update", width = "100%",
                        class = "btn btn-success", style = "margin-top: 10px;"),
   ),
        ),

        # Rightmost column: Settings and save
        column(width = 6,
           wellPanel(style = "height:430px; width:180px",
                     bigHeading("Plot settings"),
                     fluidRow(class = "num-input-row",
                        column(6, numericInput("width", "Width", value = 430, min = 100, max = 1200, step = 10)),
                        column(6, numericInput("height", "Height", value = 430, min = 100, max = 1200, step = 10))
                      ),
                      fluidRow(class = "num-input-row",
                        column(6, numericInput("cex", "Cex", value = 1.4, min = 0.5, max = 3, step = 0.1)),
                        column(6, numericInput("symbolsize", "Symbols", value = 1, min = 0.5, max = 3, step = 0.1))
                      ),
                      fluidRow(class = "num-input-row",
                        column(12, numericInput("mar", "Margins", value = 3, min = 0.1, max = 10, step = 0.1))
                      ),

                     br(),
                     checkboxGroupInput("settings", "Other options (beta)", selected  = NULL,
                                        c("Straight legs" = "straightlegs", "Arrows" = "arrows")),
                     br(),
                     fluidRow(style = "position: absolute; top:375px",
                       column(width = 6, align = "left", style = "padding-right:5px;",
                              downloadButton("savePlotPng", "PNG", class = "btn btn-info", style = "padding-left:8px;width: 100%;"),
                       ),
                       column(width = 6, align = "right", style = "padding-left:5px;",
                              downloadButton("savePlotPdf", "PDF", class = "btn btn-info", style = "padding-left:8px;width: 100%;"),
                       )
                     )
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
    mylink("changelog", "https://github.com/magnusdv/quickped/blob/master/NEWS.md"), ").",
    "QuickPed is powered by the ",
    mylink("pedsuite", "https://magnusdv.github.io/pedsuite/"),
    " and uses ",
    mylink("kinship2", "https://cran.r-project.org/package=kinship2"),
    " for alignment calculations.",
    "If you find something that isn't working properly, please file a bug report at ",
    mylink("https://github.com/magnusdv/quickped/issues"), "."),
)



server = function(input, output, session) {

  currentPedData = reactiveVal(list(ped = nuclearPed(), aff = character(0), carrier = character(0), deceased = character(0),
                                    twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0))))

  previousStack = reactiveVal(list())

  plotdat = reactiveValues(alignment = NULL, scaling = NULL, annotation = NULL)
  sel = reactiveVal(character(0))

  relText = reactiveVal(NULL)


# Update pedigree ---------------------------------------------------------


  updatePedData = function(currData, ped = NULL, aff = NULL, carrier = NULL,
                           deceased = NULL, twins = NULL, clearSel = TRUE,
                           clearInput = TRUE, clearRel = TRUE) {  #print("updatePedData")
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
    params = paramsBuiltin(choice)

    updateNumericInput(session, "width", value = params$width)
    updateNumericInput(session, "height", value = params$height)
    updateNumericInput(session, "cex", value = params$cex)
    updateNumericInput(session, "symbolsize", value = params$symbolsize)
    updateNumericInput(session, "mar", value = params$mar)

    pedDat = req(BUILTIN_PEDS[[choice]])
    if(is.ped(pedDat))
      pedDat = list(ped = pedDat)

    defaultArgs = list(currData = currentPedData(), aff = character(0), carrier = character(0),
                twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0)),
                deceased = character(0), clearInput = FALSE)

    args = modifyList(defaultArgs, pedDat)
    do.call(updatePedData, args)
  })


  observeEvent(input$loadped, {
    file = req(input$loadped$datapath)
    y = tryCatch(readPed2(file), error = errModal, warning = errModal)

    updatePedData(currentPedData(), ped = req(y$ped), aff = y$aff,
                  carrier = character(0), deceased = character(0),
                  twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0)))
  })

  observeEvent(input$randomped, {
    n = sample(5:15, size = 1)             # total pedigree size
    f = sample(2:floor((n+1)/2), size = 1) # number of founders
    ped = NULL
    while(is.null(ped)) {
      ped = tryCatch(
        randomPed(n = n, founders = f, selfing = FALSE) |> relabel(),
        error = function(e) NULL, warning = function(e) NULL)
    }
    updatePedData(currentPedData(), ped = ped, aff = character(0),
                  carrier = character(0), deceased = character(0),
                  twins = data.frame(id1 = character(0), id2 = character(0), code = integer(0)))
  })

# Modify pedigree ---------------------------------------------------------

  observeEvent(input$addson, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addChild(currData$ped, id, sex = 1),
                      error = function(e) errModal(e, html = TRUE))
    updatePedData(currData, ped = newped, clearSel = FALSE)
  })

  observeEvent(input$adddaughter, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addChild(currData$ped, id, sex = 2),
                      error = function(e) errModal(e, html = TRUE))
    updatePedData(currData, ped = newped, clearSel = FALSE)
  })

  observeEvent(input$addsibling, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addSib(currData$ped, id, sex = 1),
                      error = function(e) errModal(e, html = TRUE))
    updatePedData(currData, ped = newped, clearSel = TRUE)
  })

  observeEvent(input$addparents, {
    id = req(sel())
    currData = currentPedData()
    newped = tryCatch(addParents(currData$ped, id, verbose = FALSE),
                      error = errModal)
    updatePedData(currData, ped = newped)
  })

  observeEvent(input$swapsex, {
    currData = currentPedData()
    id = sel()
    newped = swapSex(currData$ped, id, verbose = FALSE)

    # Catch discordant swaps for MZ twins
    tw = currData$twins
    mz = tw[tw$code == 1, , drop = FALSE]
    sx1 = getSex(newped, mz$id1)
    sx2 = getSex(newped, mz$id2)
    if(any(sx1 != sx2)) {
      errModal("MZ twins must have the same sex")
      return()
    }

    updatePedData(currData, ped = newped, clearSel = length(id) > 1)
  })

  observeEvent(input$affection, {
    id = req(sel())
    currData = currentPedData()
    aff = currData$aff
    newAff = setdiff(union(aff, id), intersect(aff, id))
    updatePedData(currData, aff = newAff, clearSel = length(id) > 1, clearRel = FALSE)

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

  observeEvent(input$removeDown, {
    ids = req(sel())
    currData = currentPedData()
    newdat = tryCatch(removeSel(currData, ids, "descendants"),
                      error = errModal)
    updatePedData(currData, ped = newdat$ped, aff = newdat$aff, carrier = newdat$carr,
                  deceased = newdat$dec, twins = newdat$tw)
  })

  observeEvent(input$removeUp, {
    ids = req(sel())
    currData = currentPedData()
    newdat = tryCatch(removeSel(currData, ids, "ancestors"),
                      error = errModal)
    updatePedData(currData, ped = newdat$ped, aff = newdat$aff, carrier = newdat$carr,
                  deceased = newdat$dec, twins = newdat$tw)
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
      err = "Twins must have the same parents"
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

    # Show labels
    updateRadioButtons(session, "showlabs", selected = "show")

    # Reset settings (main plot settings reset in 'startped')
    updateCheckboxGroupInput(session, "settings", selected = character(0))
    updateCheckboxGroupInput(session, "include", selected = "head")
  })



# Labels ------------------------------------------------------------------

  output$labels = renderUI({
    labs = labels(currentPedData()$ped)
    fields = paste0("lab", seq_along(labs))
    lapply(seq_along(labs), function(i) textInput2(fields[i], value = labs[i]))
  })

  observeEvent(input$labs123, { # New labels: 1, 2, ...
    currData = currentPedData()
    newData = updateLabelsData(currData, new = "asPlot", .alignment = plotAlignment())
    do.call(updatePedData, c(list(currData = currData), newData))
  })

  observeEvent(input$labsGen, { # New labels: I-1, I-2, ...
    currData = currentPedData()
    newData = updateLabelsData(currData, new = "generations", .alignment = plotAlignment())
    do.call(updatePedData, c(list(currData = currData), newData))
  })

  observeEvent(input$updateLabs, {
    currData = currentPedData()
    oldlabs = currData$ped$ID
    fields = paste0("lab", seq_along(oldlabs))
    newlabs = vapply(fields, function(s) input[[s]], FUN.VALUE = "1") |>
      as.character() |> trimws()

    if(identical(newlabs, oldlabs))
      return()

    err = NULL
    if(dup <- anyDuplicated(newlabs))
      err = paste("Duplicated ID label: ", toString(newlabs[dup]))
    else if(0 %in% newlabs)
      err = '"0" cannot be used as label'
    else if("" %in% newlabs)
      err = "Empty label"

    if(!is.null(err)) {
      errModal(err)
      return()
    }

    newData = updateLabelsData(currData, old = oldlabs, new = newlabs)
    do.call(updatePedData, c(list(currData = currData), newData))
  })


  # Plot --------------------------------------------------------------------

  plotLabs = reactive({ #print("labs")
    ped = req(currentPedData()$ped)
    switch(input$showlabs, show = breakLabs(ped), hide = NULL)
  })

  plotAlignment = reactive({ #print("align")
    curr = currentPedData()
    arrows = "arrows" %in% input$settings
    straight = "straightlegs" %in% input$settings
    .pedAlignment(curr$ped, twins = curr$twins, arrows = arrows,
                  align = if(straight) c(0,0) else c(1.5,2))
  })

  plotAnnotation = reactive({ #print("annot")
    curr = req(currentPedData())
    labs = plotLabs()
    .pedAnnotation(curr$ped, labs = labs, aff = curr$aff, carrier = curr$carrier,
                   deceased = curr$deceased, col = list(red = sel()),
                   lwd = list(`3` = sel()))
  })

  plotScaling = reactive({ #print("scaling")
    # React to these
    checknum(input$width, "Width", min = 10, max = Inf)
    checknum(input$height, "Heigth", min = 10, max = Inf)

    align = req(plotAlignment())
    annot = list(textUnder = plotLabs())
    cex = checknum(input$cex, "Cex", min = 0.01, max = 10)
    symsize = checknum(input$symbolsize, "Symbols", min = 0.01, max = 10)
    mar = checknum(input$mar, "Margins", min = 0, max = 10)

    .pedScaling(align, annot, cex = cex, symbolsize = symsize, margins = rep(mar, 4))
  })

  output$plot = renderPlot({ #print("plot")
    dat = tryCatch({
        align = withCallingHandlers(
          plotAlignment(),
          warning = function(w) {
            if(startsWith(w$message, "Unexpected result in autohint"))
              invokeRestart("muffleWarning")
          }
        )
        if(anyNA(align$x))
          stop2("Sorry, for some reason this pedigree did align properly.")
        drawPed(align, annotation = plotAnnotation(), scaling = plotScaling())
      },
      error = errModal)

    req(dat) # if unsuccessful, return gracefully
    box("outer", col = 1)
    },
    execOnResize = TRUE, res = 72, # default; seems ok in practice
    width = function() max(c(1, abs(input$width)), na.rm = TRUE),
    height = function() max(c(1, abs(input$height)), na.rm = TRUE)
  )

  positionDf = reactive({
    align = req(plotAlignment())
    scale = plotScaling()
    mat = cbind(x = align$xall,
                y = align$yall + scale$boxh/2,
                idInt = align$plotord)
    as.data.frame(mat)
  })

  observeEvent(input$ped_click, {
    posDf = positionDf()
    idInt = nearPoints(posDf, input$ped_click, xvar = "x", yvar = "y",
                       threshold = 20, maxpoints = 1)$idInt
    if(length(idInt) == 0)
      return()

    currData = currentPedData()
    id = currData$ped$ID[idInt]

    currSel = sel()
    if(id %in% currSel)
      sel(setdiff(currSel, id))
    else
      sel(c(currSel, id))
  })

  observeEvent(input$ped_dblclick, {
    posDf = positionDf()
    idInt = nearPoints(posDf, input$ped_dblclick, xvar = "x", yvar = "y",
                       threshold = 20, maxpoints = 1)$idInt
    if(length(idInt) == 0)
      return()

    currData = currentPedData()
    ped = currData$ped
    id = labels(ped)[idInt]

    # Only continue if a leaf is selected
    req(id %in% leaves(ped))

    # 1,2 --> 0; 0 --> 1
    newsex = if(getSex(ped, id) > 0) 0 else 1
    newped = setSex(ped, ids = id, sex = newsex)
    updatePedData(currData, ped = newped, clearSel = FALSE)
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
                  quote = FALSE, sep = "\t", fileEncoding = "UTF-8")
    }
  )

  output$savePlotPng = downloadHandler(
    filename = "quickped.png",
    content = function(con) {
      png(con, width = input$width, height = input$height)
      drawPed(plotAlignment(), annotation = plotAnnotation(), scaling = plotScaling())
      dev.off()
    },
    contentType = "image/png"
  )

  output$savePlotPdf = downloadHandler(
    filename = "quickped.pdf",
    content = function(file) {
      cairo_pdf(file, width = input$width/72, height = input$height/72)
      drawPed(plotAlignment(), annotation = plotAnnotation(), scaling = plotScaling())
      dev.off()
    },
    contentType = "application/pdf"
  )

# Relationships ------------------------------------------------

  dlg = modalDialog(
    h3(id = "title-h3", "Download table of pairwise coefficients"),
    tags$style(HTML("#title-h3 {background-color: gray; color: white; padding: 15px}")),
    textInput("coeffIds", label = "Individuals:", value = "", width = "100%"),
    br(),
    fluidRow(
      column(width = 6,
             checkboxGroupInput("coeffSelect", "Coefficients:",
                       c("Inbreeding" = "f",
                         "Kinship" = "phi",
                         "Degree" = "deg",
                         "IBD (kappa)" = "kappa",
                         "Identity (condensed)" = "identity",
                         "Identity (detailed)" = "detailed"
                       ),
                       selected  = c("f", "phi", "deg", "kappa"))
             ),
      column(width = 6,
             checkboxGroupInput("coeffChrom", "Chromosome:",
                                c("Autosomal" = "aut",
                                  "X-chromosomal" = "X"
                                ),
                                selected  = c("aut")),
             checkboxGroupInput("coeffInclude", "Include:",
                                c("Self relationships" = "self"),
                                selected  = NULL)
             )
      ),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      downloadButton("saveCoeffTable", "Download", class = "btn btn-info")
    )
  )

  observeEvent(input$coeffTable, {
    currData = currentPedData()
    ped = currData$ped
    ids = sel()

    if(hasMZtwins(currData)) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    if(!length(ids))
      ids = labels(ped)
    updateTextInput(session, "coeffIds", value = toString(sortIds(ped, ids)))
    tryCatch(showModal(dlg), error = errModal)
  })



  # Action when clicking "Download" in the modal dialog
  output$saveCoeffTable = downloadHandler(
    filename = "quickped-relatedness.txt",
    content = function(file) {
      tryCatch({
        ids = trimws(strsplit(req(input$coeffIds), ",")[[1]])
        ped = currentPedData()$ped

        # Coefficients selected
        coeff = input$coeffSelect

        # Autosomal and/or X?
        chr = input$coeffChrom
        if(!length(chr))
          stop("Please select chromosome type")
        Xchrom = if(length(chr) == 2) NA else chr == "X"

        # Self relationships?
        self = "self" %in% input$coeffInclude

        # Compute table
        tab = coeffTable(ped, ids, coeff = coeff, Xchrom = Xchrom, self = self)

        # Write to file
        write.table(tab, file = file, col.names = TRUE, row.names = FALSE,
                    quote = FALSE, sep = "\t", fileEncoding = "UTF-8")

        # Close window
        removeModal()
      },
      error = errModal)
    }
  )

  ### Triangle plot

  observeEvent(input$triangle, {
    currData = currentPedData()
    ped = currData$ped
    ids = sel()

    if(hasMZtwins(currData)) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    if(!length(ids))
      ids = labels(ped)

    if(length(ids) == 1) {
      relText("Please select at least 2 individuals (or none).")
      return()
    }

    if(any(ribd::inbreeding(ped, ids) > 0))
      relText(c("Note: Kappa undefined for inbred individuals"))

    showModal(modalDialog(
      h3("Relatedness triangle", align = "center"),
      plotOutput("plotTriangle", width = "560px", height = "480px"),
      footer = tags$div(
        tags$div(
          style = "float: left; margin-left:10px; margin-top:5px",
          radioButtons("trianglemode", NULL, inline = TRUE, selected = "noninbred",
                       choices = c("Noninbred (κ)" = "noninbred",
                                   "Inbred (Δ)" = "inbred")),
        ),
        tags$div(
          style = "float: right",
          modalButton("Cancel"),
          downloadButton("saveTriangle", "Download", class = "btn btn-info")
        )
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  output$plotTriangle = renderPlot(
    plotKappa(currentPedData()$ped, ids = sel(), mode = input$trianglemode),
    width = 560, height = 480,
    res = 72 # to low, but increasing it disturbs everything else
  )

  output$saveTriangle = downloadHandler(
    filename = "triangle.png",
    content = function(file) {
      png(file, width = 560*2, height = 480*2, res = 72*2)
      plotKappa(currentPedData()$ped, ids = sel(), mode = input$trianglemode)
      dev.off()
    },
    contentType = "image/png"
  )

  observeEvent(input$describe, {
    currData = currentPedData()
    ped = req(currData$ped)
    ids = sortIds(ped, ids = sel())

    if(length(ids) != 2) {
      relText("Please select exactly 2 individuals.")
      return()
    }

    if(hasMZtwins(currData)) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    paths = verbalisr::verbalise(ped, ids)
    txt = format(paths)
    txt = gsub("([[:graph:]])  ([[:graph:]])", "\\1 \\2", txt) # remove double spaces
    relText(txt)
  })



  observeEvent(input$coeffs, {
    currData = currentPedData()
    ped = req(currData$ped)
    ids = sortIds(ped, ids = sel())

    N = length(ids)
    if(!N %in% 1:2) {
      relText(c("Please select 1 or 2 individuals.",
                "(Or click the Table button for more options.)"))
      return()
    }

    if(hasMZtwins(currData)) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    if(N == 1) {
      txt = c(sprintf("Inbreeding coefficient for individual %s:", ids),
              sprintf("* f = %g", ribd::inbreeding(ped, ids)))
      relText(txt)
      return()
    }

    ### N = 2
    txt =  sprintf("Relatedness coefficients for %s and %s:", ids[1], ids[2])

    # Inbreeding
    inb = ribd::inbreeding(ped, ids)
    txt = c(txt,
            sprintf("* Inbreeding: f1 = %.4g", inb[1]),
            sprintf("              f2 = %.4g", inb[2]))

    # Kinship
    phi = ribd::kinship(ped, ids)
    txt = c(txt,
            sprintf("* Kinship:   phi = %.4g", phi))

    # Degree
    deg = ribd::kin2deg(phi, unrelated = NA)
    txt = c(txt,
            sprintf("* Degree:    deg = %.4g", deg))

    # Kappa (if both outbred) or Delta
    if(all(inb == 0)) {
      kap = ribd::kappaIBD(ped, ids, simplify = TRUE) # simplify to vector of len 3
      kap = sprintf("%.4g", round(kap, 4))
      txt = c(txt,
            sprintf("* IBD:     kappa = (%s)", toString(kap)))
    }
    else {
      delta = ribd::condensedIdentity(ped, ids)
      delta = sprintf("%.4g", round(delta, 4))
      len = nchar(toString(delta))
      if(len <= 50)
        s = sprintf("* Condensed identity =\n  (%s)", toString(delta))
      else
        s = sprintf("* Condensed identity = (%s\n  %s)",
                    toString(delta[1:3]), toString(delta[4:9]))

      txt = c(txt, s)
    }

    relText(txt)
  })

  output$description = renderText(req(relText()), sep = "\n")
}


shinyApp(ui = ui, server = server)
