suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(pedtools)
})


ui = fluidPage(

  useShinyjs(),  # Set up shinyjs

  tags$head(
    tags$style(
      HTML('
        .well {padding-top: 10px}
        .btn-default {margin: 2px;}
        .form-group {margin-bottom: 3px;}
        .form-control {padding-top, padding-bottom: 1px; height: 28px;}
        .control-label {margin-bottom: 0px;}
        .irs-min, .irs-max, .irs-single {display:none; line-height: 0px;}
        .irs-bar, .irs-bar-edge, .irs-line  {top: 12px}
        .irs-slider {top: 4px; colour: orange}
        .irs {height: 30px}
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
    em("Add son", .noWS = "outside"), " or ", em("Add daughter", .noWS = "outside"),
    ". If needed, replace the default labels by other names, and use the ",
    em("Toggle aff", .noWS = "outside"),
    " button to switch affection status of selected individuals."),

  br(),

  fluidRow(

    # Sidebar
    column(width = 6, style = "max-width:450px",

      fluidRow(

        # First control column
        column(width = 6,
          wellPanel(style = "height:400px",
            h4(strong("Plot settings"), .noWS = "before"),
            sliderInput("width", "Width", ticks = FALSE, min = 100, max = 1000, value = 400, step = 1),
            sliderInput("height", "Height", ticks = FALSE, min = 100, max = 1000, value = 400, step = 1),
            sliderInput("cex", "Expansion", ticks = FALSE, min = 0.5, max = 3, value = 1.5, step = 0.1),
            sliderInput("symbolsize", "Symbol size", ticks = FALSE, min = 0.5, max = 3, value = 1, step = 0.1),
            sliderInput("mar", "Margins", ticks = FALSE, min = 0, max = 10, value = 3, step = 0.1),
            br(),
            downloadButton("savePlot", "Save plot", class = "btn btn-info", style = "width: 100%")
          ),

          wellPanel(
            h4(strong("Labels"), .noWS = "before"),
            uiOutput("labels"),
            actionButton("updateLabs", "Update", width = "100%",
                         class = "btn btn-success", style = "margin-top: 10px"),
          )
        ),

        # Second control column
        column(width = 6,
          wellPanel(style = "height:400px",
            h4(strong("Build pedigree"), .noWS = "before"),
            actionButton("addson", "Add son", width = "100%"),
            actionButton("adddaughter", "Add daughter", width = "100%"),
            actionButton("addparents", "Add parents", width = "100%"),
            actionButton("swapsex", "Toggle sex", width = "100%"),
            actionButton("affection", "Toggle aff", width = "100%"),
            actionButton("remove", "Remove", width = "100%"),
            div(style="margin-bottom:20px"),
            disabled(actionButton("undo", "Undo", width = "100%", class = "btn btn-warning")),
            actionButton("reset", "Reset", width = "100%", class = "btn btn-danger"),
          ),
          wellPanel(
            h4(strong("Ped file")),

            checkboxGroupInput("include", "Include", selected  = c("head", "aff"),
                               c("Headers" = "head", "Family ID" = "famid", "Affection status" = "aff")),
            downloadButton("savePed", "Save ped file", class="btn btn-info", style = "width: 100%"),
          )
        )
      )
    ),

    # Plot window
    mainPanel(width = 6,
              plotOutput("plot", click = "ped_click", width = "auto"),
    )
  ),

  p("QuickPed is based on the R package ",
    a("pedtools", href = "https://CRAN.R-project.org/package=pedtools", .noWS = "outside"), ".",
    " The plotting is powered by ",
    a("kinship2", href = "https://CRAN.R-project.org/package=kinship2", .noWS = "outside"), "."),

  p("Bug reports are welcome at ", a("https://github.com/magnusdv/quickped/issues",
                                     href = "https://github.com/magnusdv/quickped/issues",
                                     .noWS = "outside"), "."),
)



server = function(input, output, session) {

  # Reactive values
  currentPed = reactiveVal(nuclearPed(1))
  prevPed = reactiveVal(nuclearPed(1))
  pdat = reactiveVal(NULL)
  sel = reactiveVal(character(0))
  affected = reactiveVal(character(0))

  plotArgs = reactive({
    m = input$mar
    adjmar = c(max(m - 1, 0), m, m + 1, m)
    list(cex = input$cex, symbolsize = input$symbolsize, mar = adjmar, aff = affected())
  })

  output$labels = renderUI({
    labs = labels(currentPed())
    fields = paste0("lab", seq_along(labs))
    lapply(seq_along(labs), function(i)
      textInput(fields[i], label = NULL, value = labs[i], width = "100%"))
  })

  observeEvent(input$updateLabs, {
    currPed = currentPed()
    currAff = affected()
    newlabs = vapply(paste0("lab", 1:pedsize(currPed)),
                     function(s) input[[s]], FUN.VALUE = "1")
    newped = tryCatch(relabel(currPed, new = newlabs),
                      error = function(e) errModal(e))
    if(is.null(newped))
      return()

    currentPed(newped)
    newAff = labels(newped)[internalID(currPed, currAff)]
    affected(newAff)
    sel(character(0))
  })

  output$plot = renderPlot({
    args = plotArgs()
    dat = tryCatch(
      plot(currentPed(), col = list(red = sel()), cex = args$cex,
           symbolsize = args$symbolsize, aff = args$aff, margins = args$mar),
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
      currPed = currentPed()
      inclHead = "head" %in% input$include
      inclFamid = "famid" %in% input$include
      inclAff = "aff" %in% input$include

      df = as.data.frame(currPed)
      if(inclFamid)
        df = cbind(famid = 1, df)
      if(inclAff)
        df = cbind(df, aff = ifelse(labels(currPed) %in% affected(), 2, 1))

      write.table(df, file = con, col.names = inclHead, row.names = FALSE,
                  quote = FALSE, sep = "\t")
    }
  )

  output$savePlot = downloadHandler(
    filename = "quickped.png",
    content = function(con) {
      args = plotArgs()
      png(con, width = input$width, height = input$height)
      plot(currentPed(), aff = args$aff, cex = args$cex,
           symbolsize = args$symbolsize, margins = args$mar)
      dev.off()
    },
    contentType = "image/png"
  )

  observeEvent(input$ped_click, {
    symbolDat = pdat2df(pdat(), labs = labels(currentPed()))

    id = nearPoints(symbolDat, input$ped_click, xvar = "x", yvar = "y",
                    threshold = 20, maxpoints = 1)$id
    if(!length(id))
      return()

    current = sel()
    if(id %in% current)
      sel(setdiff(current, id))
    else
      sel(c(current, id))
  })

  observeEvent(input$addson, {
    id = sel()
    if(length(id) == 0)
      return()
    currPed = currentPed()
    newped = addChild(currPed, sel(), sex = 1)
    if(is.null(newped))
        return()
    currentPed(newped)
    prevPed(currPed)
    enable("undo")
  })

  observeEvent(input$adddaughter, {
    id = sel()
    if(length(id) == 0)
      return()
    currPed = currentPed()
    newped = addChild(currPed, sel(), sex = 2)
    if(is.null(newped))
      return()

    currentPed(newped)
    prevPed(currPed)
    enable("undo")
  })

  observeEvent(input$addparents, {
    id = sel()
    if(length(id) == 0)
      return()
    currPed = currentPed()
    newped = tryCatch(addParents(currPed, id, verbose = FALSE),
             error = function(e) errModal(e))
    if(is.null(newped))
      return()

    currentPed(newped)
    prevPed(currPed)
    enable("undo")
    sel(character(0))
  })

  observeEvent(input$swapsex, {
    currPed = currentPed()
    newped = swapSex(currPed, sel(), verbose = FALSE)
    currentPed(newped)
    prevPed(currPed)
    enable("undo")
    sel(character(0))
  })

  observeEvent(input$remove, {
    id = sel()
    if(length(id) == 0)
      return()
    currPed = currentPed()
    newped = removeIndividuals(currPed, id, verbose = FALSE)
    if(is.null(newped))
      return()
    currentPed(newped)
    prevPed(currPed)
    affected(setdiff(affected(), id))
    enable("undo")
    sel(character(0))
  })

  observeEvent(input$affection, {
    id = sel()
    if(length(id) == 0)
      return()
    currAff = affected()
    affected(setdiff(union(currAff, id), intersect(currAff, id)))
    sel(character(0))
  })

  observeEvent(input$undo, {
    currentPed(prevPed())
    prevPed(nuclearPed())
    sel(character(0))
    disable("undo")
  })

  observeEvent(input$reset, {
    currPed = currentPed()
    currentPed(nuclearPed())
    prevPed(currPed)
    sel(character(0))
    affected(character(0))
  })


}


shinyApp(ui = ui, server = server)
