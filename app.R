suppressPackageStartupMessages({
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(pedtools)
  library(ribd)
  library(verbalisr)
  library(ggplot2)
  library(ggrepel)
  library(glue)
  library(lubridate)
})

VERSION = "4.1.1"
DEBUG = F; debugCounter = 0
.debug <<- function(msg) if(DEBUG) cat(debugCounter <<- debugCounter+1, msg, "\n")

ui = fluidPage(

  includeCSS("www/custom.css"),
  tags$head(includeHTML("GA.html")),
  tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lobster&display=swap")),
  useShinyjs(),

  # tags$div(id = "banner",
  #       p(id="big-text", "New app design!"),
  #       p("Discover the ", mylink("new features", href="https://github.com/magnusdv/quickped/blob/master/NEWS.md", style = "font-weight:bold;")),
  #       p(id="small-text", "Or stay with the old version: ", mylink("QuickPed3", href="https://magnusdv.shinyapps.io/quickped-32/"))
  # ),


  # Application title
  h2(id = "title-h2", "QuickPed: An Interactive Pedigree Creator"),
  tags$style(HTML("#title-h2 {background-color: #f0f4f7; color: #505050; padding: 15px}")),

  p(bold("Purpose: "), style = "margin: 0 0 5px",
    "QuickPed lets you rapidly create attractive pedigree plots, save them as images or text files, and analyse the relationships within them."),

  p(bold("Instructions: "), style = "margin: 0 0 5px",
    "Choose a suitable start pedigree and modify it by clicking on individuals and using appropriate buttons.
    For example, to add a male child, select the parent(s) and press the ",
    img(src = "add-son.svg", height = "18px", width = "18px", style="border: 1px solid gray; border-radius: 3px;"), "icon.",
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
            div(style = "display: flex; flex-wrap: nowrap; align-items: center; width: 100%; justify-content: space-between;",
              bigHeading("Modify"),
              actionButton("clearsel", label = NULL,
                icon = icon(name = NULL, class = "custom_icon",
                            style = "background-image: url('hand-pointer-strikethrough.svg'); width: 20px; aspect-ratio: 1/1;"),
                class = "icon_button",
                style = "float:left"),
              bsTooltip("clearsel", HTML("Deselect<br>all")),
            ),
            midHeading("Add", style = "margin-top: 2.5px"),
            fluidRow(style = "margin-left:0px; width: 170px",
              iconButton("addson", icon = "add-son.svg"),
              iconButton("adddaughter", icon = "add-daughter.svg"),
              iconButton("addsibRight", icon = "add-sib.svg"),
              iconButton("addsibLeft", icon = "add-sib-left.svg"),
              iconButton("addparents", icon = "add-parents.svg"),
            ),
            bsTooltip("addson", "Add son"),
            bsTooltip("adddaughter", "Add daughter"),
            bsTooltip("addsibRight", "Add sibling to the right"),
            bsTooltip("addsibLeft", "Add sibling to the left"),
            bsTooltip("addparents", "Add parents"),
            midHeading("Sex"),
            fluidRow(style = "margin-left:0px; width: 170px",
              iconButton("sex1", icon = "sex-male.svg"),
              iconButton("sex2", icon = "sex-female.svg"),
              iconButton("sex0", icon = "sex-unknown.svg"),
              iconButton("sex3", icon = "sex-miscarriage.svg"),
              bsTooltip("sex1", "Male"),
              bsTooltip("sex2", "Female"),
              bsTooltip("sex0", "Unknown"),
              bsTooltip("sex3", "Miscarriage"),
            ),
            midHeading("Style"),
            fluidRow(style = "margin-left:0px; width: 170px",
              iconButton("clean", icon = "open.svg"),
              iconButton("hatched", icon = "hatched.svg"),
              iconButton("carrier", icon = "carrier.svg"),
              iconButton("deceased", icon = "deceased.svg"),
              iconButton("dashed", icon = "dashed.svg"),
            ),

            midHeading("Fill"),
            fluidRow(style = "margin-left:0px; width: 170px",
              iconButton("fill-white", icon = "fill-white.svg"),
              iconButton("fill-black", icon = "fill-black.svg"),
              iconButton("fill-red", icon = "fill-red.svg"),
              iconButton("fill-green", icon = "fill-green.svg"),
              iconButton("fill-blue", icon = "fill-blue.svg"),
            ),
            fluidRow(style = "margin-left:0px; width: 170px",
              iconButton("fill-pink", icon = "fill-pink.svg"),
              iconButton("fill-cyan", icon = "fill-cyan.svg"),
              iconButton("fill-magenta", icon = "fill-magenta.svg"),
              iconButton("fill-yellow", icon = "fill-yellow.svg"),
              iconButton("fill-gray", icon = "fill-gray.svg"),
            ),
            fluidRow(
              column(6,
                midHeading("Twins"),
                actionButton("twinstatus", "MZ / DZ", width = "100%",
                  style = "padding-top: 5px; padding-bottom: 5px; padding-left: 0px; padding-right: 3px"),
                bsTooltip("twinstatus", HTML("Change twin status"), placement = "top"),
              ),
              column(6,
                 midHeading("Remove"),
                 fluidRow(
                   pedButton("removeDown", NULL, icon("arrow-down"), side = "left"),
                   pedButton("removeUp", NULL, icon("arrow-up"), side = "right"),
                   bsTooltip("removeDown", HTML("Remove&nbsp;selected + descendants"), placement = "top"),
                   bsTooltip("removeUp", HTML("Remove&nbsp;selected + ancestors"), placement = "top"),
                  ),
              ),
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
           p("Double-click on an individual to add text", style = "width: 100%; font-size: smaller; text-align: left;")
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
                     checkboxGroupInput("settings", "Other options (beta)", selected = NULL,
                                        c("Straight legs" = "straightlegs", "Arrows" = "arrows")),

                     actionButton("rcode", "R code", style = "background: lightgoldenrodyellow; font-family:monospace; font-weight: bolder; padding:0px; width: 100%;"),
                     fluidRow(style = "position: absolute; top:375px",
                       column(width = 6, align = "left", style = "padding-right:5px;",
                              downloadButton("savePlotPng", "PNG", class = "btn btn-info", style = "padding-inline:8px; width:100%;"),
                       ),
                       column(width = 6, align = "right", style = "padding-left:5px;",
                              downloadButton("savePlotPdf", "PDF", class = "btn btn-info", style = "padding-inline:8px; width:100%;"),
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

  pedigree = reactiveValues(ped = nuclearPed(1), twins = NULL, miscarriage = NULL)
  styles = reactiveValues(hatched = NULL, carrier = NULL, deceased = NULL,
                          dashed = NULL, fill = NULL)
  textAnnot = reactiveVal(NULL)
  sel = reactiveVal(character(0))
  relText = reactiveVal(NULL)


  # Stack of previous states ------------------------------------------------

  previousStack = reactiveVal(list())
  observe(switch(length(previousStack()) + 1, disable("undo"), enable("undo")))

  addCurrentToStack = function() {  .debug("add to stack")
    stack = previousStack()
    n = length(stack)

    # Collect state of current reactives in static list
    curr = c(reactiveValuesToList(pedigree), reactiveValuesToList(styles),
             list(textAnnot = textAnnot()))

    # If new: add
    if(n == 0 || !identical(curr, stack[[n]]))
      previousStack(c(stack, list(curr)))
  }

  # Main updating function --------------------------------------------------

  updatePed = function(..., addToStack = TRUE, clearSel = NULL,
                       clearStart = NULL, clearRel = NULL) {  .debug("updateped")
    if(!length(args <- list(...)))
      return()

    # Push current state to stack
    if(addToStack)
      addCurrentToStack()

    # Update only given attributes
    argnames = names(args)

    # Convert empty vectors to explicit NULLs
    args = lapply(args, function(b) if(length(b)) b else NULL)

    # Update reactives
    for(a in .myintersect(argnames, c("ped", "twins", "miscarriage")))
      pedigree[[a]] = args[[a]]

    for(a in .myintersect(argnames, names(styles)))
      styles[[a]] = args[[a]]

    if("textAnnot" %in% argnames)
      textAnnot(args$textAnnot)

    # Clear stuff
    if(clearSel %||% ("textAnnot" %in% argnames))
      sel(character(0))

    if(clearRel %||% any(c("ped", "twins") %in% argnames))
      relText(NULL)

    if(clearStart %||% any(c("ped", "twins") %in% argnames))
      updateSelectInput(session, "startped", selected = "")

    codeTxt(NULL)
  }

  # Reset with a new pedigree  -----------------------------------------------

  resetPed = function(ped, ...) {  .debug("resetped")
    cleanArgs = list(twins = NULL, miscarriage = NULL, hatched = NULL,
                     carrier = NULL, dashed = NULL,
                     deceased = NULL, fill = NULL, textAnnot = NULL)
    args = modifyList(cleanArgs, list(ped = ped, ...), keep.null = TRUE)
    do.call(updatePed, args)
  }


  # Startped/load -----------------------------------------------------------

  observeEvent(input$startped, {   .debug("startped")
    choice = req(input$startped)
    params = paramsBuiltin(choice)

    updateNumericInput(session, "width", value = params$width)
    updateNumericInput(session, "height", value = params$height)
    updateNumericInput(session, "cex", value = params$cex)
    updateNumericInput(session, "symbolsize", value = params$symbolsize)
    updateNumericInput(session, "mar", value = params$mar)

    y = req(BUILTIN_PEDS[[choice]])
    args = list(ped = y$ped %||% y,
                fill = modifyVec(NULL, y$aff, val = 1),
                carrier = y$carrier,
                clearStart = FALSE)
    do.call(resetPed, args)
  }, ignoreInit = TRUE)

  observeEvent(input$loadped, {   .debug("loadped")
    file = req(input$loadped$datapath)
    y = tryCatch(readPed2(file), error = errModal, warning = errModal)
    resetPed(ped = y$ped, fill = modifyVec(NULL, y$aff, val = 1))
  })

  observeEvent(input$randomped, {   .debug("random ped")
    n = sample(5:15, size = 1)             # total pedigree size
    f = sample(2:floor((n+1)/2), size = 1) # number of founders
    ped = NULL
    while(is.null(ped)) {
      ped = tryCatch(randomPed(n, f, maxDirectGap = 0) |> relabel(),
                     error = function(e) NULL, warning = function(e) NULL)
    }
    resetPed(ped = ped)
  })

# Modify pedigree ---------------------------------------------------------

  observeEvent(input$addson, {    .debug("add son")
    id = req(sel())
    tryCatch({
      if(id %in% pedigree$miscarriage)
        stop2("Cannot add children to a miscarriage")
      updatePed(ped = addSon(pedigree$ped, id, verbose = FALSE))
    }, error = errModal)
  })

  observeEvent(input$adddaughter, {  .debug("add daughter")
    id = req(sel())
    tryCatch({
      if(id %in% pedigree$miscarriage)
        stop2("Cannot add children to a miscarriage")
      updatePed(ped = addDaughter(pedigree$ped, id, verbose = FALSE))
    }, error = errModal)
  })

  observeEvent(input$addsibRight, {  .debug("add right sib")
    id = req(sel())
    tryCatch({
      updatePed(ped = addSib(pedigree$ped, id, side = "right"))
    }, error = errModal)
  })

  observeEvent(input$addsibLeft, {  .debug("add left sib")
    id = req(sel())
    tryCatch({
      updatePed(ped = addSib(pedigree$ped, id, side = "left"))
    }, error = errModal)
  })

  observeEvent(input$addparents, {  .debug("add parents")
    ids = req(sel()) # If multiple, the first is interpreted as child, followed by parents
    ok = FALSE
    tryCatch({
      updatePed(ped = addPar(pedigree$ped, ids))
      ok = TRUE
    }, error = errModal)

    if(ok && length(ids) %in% 2:3 && !isTRUE(input$suppressParentMessage))
      showModal(parentMessage)
  })

  observeEvent(input$sex1, {    .debug("sex1")
    id = req(sel())
    tryCatch({
      ped = changeSex(pedigree$ped, id, sex = 1, twins = pedigree$twins)
      updatePed(ped = ped, miscarriage = .mysetdiff(pedigree$miscarriage, id))
    }, error = errModal)
  })

  observeEvent(input$sex2, {   .debug("sex2")
    id = req(sel())
    tryCatch({
      ped = changeSex(pedigree$ped, id, sex = 2, twins = pedigree$twins)
      updatePed(ped = ped, miscarriage = .mysetdiff(pedigree$miscarriage, id))
    }, error = errModal)
  })

  observeEvent(input$sex0, {  .debug("sex0")
    id = req(sel())
    tryCatch({
      ped = changeSex(pedigree$ped, id, sex = 0, twins = pedigree$twins)
      updatePed(ped = ped, miscarriage = .mysetdiff(pedigree$miscarriage, id))
    }, error = errModal)
  })

  observeEvent(input$sex3, {   .debug("miscarriage")
    id = req(sel())
    tryCatch({
      if(!all(id %in% leaves(pedigree$ped)))
        stop2("A parent cannot be a miscarriage")
      updatePed(miscarriage = union(pedigree$miscarriage, id), clearSel = TRUE)
    }, error = errModal)
  })

  observeEvent(input$clean, {   .debug("clear styles")
    id = req(sel())
    addCurrentToStack()
    styles$hatched = setdiff(styles$hatched, id)
    styles$carrier = setdiff(styles$carrier, id)
    styles$deceased = setdiff(styles$deceased, id)
    styles$dashed = setdiff(styles$dashed, id)
  })

  observeEvent(input$hatched, {    .debug("hatched")
    updatePed(hatched = union(styles$hatched, req(sel())))
  })

  observeEvent(input$carrier, {  .debug("carrier")
    updatePed(carrier = union(styles$carrier, req(sel())))
  })

  observeEvent(input$deceased, {  .debug("deceased")
    updatePed(deceased = union(styles$deceased, req(sel())))
  })

  observeEvent(input$dashed, {  .debug("dashed")
    updatePed(dashed = union(styles$dashed, req(sel())))
  })

  COLS = c(white = 0, black = 1, red = 2, green = 3, blue = 4, cyan = 5,
           magenta = 6, yellow = 7, gray = 8, pink = "pink")
  for(cc in names(COLS)) local({
    thisCol = cc
    tag = paste0("fill-", thisCol)
    observeEvent(input[[tag]], {   .debug(tag)
      fill = modifyVec(styles$fill, req(sel()), val = COLS[[thisCol]])
      updatePed(fill = fill)
     })
  })


  # Remove individuals ------------------------------------------------------

  observeEvent(input$removeDown, {  .debug("remove down")
    ids = req(sel())
    dat = c(reactiveValuesToList(pedigree), reactiveValuesToList(styles),
            list(textAnnot = textAnnot()))
    tryCatch({
      newdat = removeSel(dat, ids = ids, updown = "descendants")
      do.call(updatePed, newdat)
    }, error = errModal)
  })

  observeEvent(input$removeUp, {   .debug("remove down")
    ids = req(sel())
    dat = c(reactiveValuesToList(pedigree), reactiveValuesToList(styles),
            list(textAnnot = textAnnot()))
    tryCatch({
      newdat = removeSel(dat, ids = ids, updown = "ancestors")
      do.call(updatePed, newdat)
    }, error = errModal)
  })

  observeEvent(input$twinstatus, {   .debug("twinstatus")
    ids = req(sel())
    tryCatch({
      updatePed(twins = updateTwins(pedigree$ped, pedigree$twins, ids))
    }, error = errModal)
  })

  observeEvent(input$clearsel, sel(character(0)))


  # Undo/reset --------------------------------------------------------------

  observeEvent(input$undo, {    .debug("undo")
    stack = previousStack()
    len = length(stack)
    if(len == 0)
      return()

    args = c(stack[[len]], list(addToStack = FALSE))
    do.call(updatePed, args)

    previousStack(stack[-len])
  })

  observeEvent(input$reset, {    .debug("reset")
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

  output$labels = renderUI({     .debug("labelsUI")
    labs = labels(pedigree$ped)
    n = length(labs)
    fields = paste0("lab", seq_len(n))
    h = if(n < 10) 24 else if (n < 20) 21 else 18
    lapply(seq_along(labs), function(i) textInput2(fields[i], value = labs[i], height = h))
  })

  observeEvent(input$labs123, { # New labels: 1, 2, ...
    newdat = updateLabelsData(pedigree, styles, textAnnot(), new = "asPlot", .alignment = plotAlignment())
    newdat$clearSel = newdat$clearRel = TRUE
    do.call(updatePed, newdat)
  })

  observeEvent(input$labsGen, { # New labels: I-1, I-2, ...
    newdat = updateLabelsData(pedigree, styles, textAnnot(), new = "generations", .alignment = plotAlignment())
    newdat$clearSel = newdat$clearRel = TRUE
    do.call(updatePed, newdat)
  })

  observeEvent(input$updateLabs, {  .debug("updatelabs")
    oldlabs = labels(pedigree$ped)
    fields = paste0("lab", seq_along(oldlabs))
    newlabs = vapply(fields, function(s) input[[s]], FUN.VALUE = "1") |>
      as.character() |> trimws()

    if(identical(oldlabs, newlabs))
      return()

    tryCatch({
      newdat = updateLabelsData(pedigree, styles, textAnnot(), new = newlabs)
      newdat$clearSel = newdat$clearRel = TRUE
      do.call(updatePed, newdat)
    }, error = errModal)
  })

  # Plot --------------------------------------------------------------------

  plotLabs = reactive({  .debug("plotlabs")
    ped = pedigree$ped
    switch(input$showlabs, show = breakLabs(ped), hide = NULL)
  })

  plotAlignment = reactive({  .debug("align")
    arrows = "arrows" %in% input$settings
    straight = "straightlegs" %in% input$settings

    # Safety check (should be avoided elsewhere)
    if(is.pedList(pedigree$ped))
      stop2("Disconnected pedigree")

    .pedAlignment(pedigree$ped, twins = pedigree$twins, arrows = arrows,
                  miscarriage = pedigree$miscarriage,
                  align = if(straight) c(0,0) else c(1.5,2))
  })

  plotAnnotation = reactive({ .debug("annot")
    .pedAnnotation(req(pedigree$ped),
                   labs = plotLabs(),
                   hatched = styles$hatched, hatchDensity = 20,
                   carrier = styles$carrier,
                   deceased = styles$deceased,
                   textAnnot = formatAnnot(textAnnot(), input$cex - 0.2),
                   col = list(red = sel()),
                   fill = styles$fill %||% NA,
                   lty = list(dashed = styles$dashed),
                   lwd = list(`3` = sel(),
                              `1.5` = .mysetdiff(styles$dashed, sel())))
  })

  plotScaling = reactive({  .debug("scaling")
    checknum(input$width, "Width", min = 100, max = Inf)
    checknum(input$height, "Heigth", min = 100, max = Inf)

    align = req(plotAlignment())
    annot = list(textUnder = plotLabs())
    cex = checknum(input$cex, "Cex", min = 0.01, max = 10)
    symsize = checknum(input$symbolsize, "Symbols", min = 0.01, max = 10)
    mar = checknum(input$mar, "Margins", min = 0, max = 10)

    .pedScaling(align, annot, cex = cex, symbolsize = symsize, margins = rep(mar, 4))
  })

  output$plot = renderPlot({    .debug("plot")

    # Plot, but catch various errors & warnings
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
        scale = plotScaling()
        annot = plotAnnotation()
        drawPed(align, annotation = annot, scaling = scale)
      },
      error = errModal) |> req()

      box("outer", col = 1)
    },
    execOnResize = TRUE, res = 72, # default; seems OK in practice
    width = function() max(c(100, abs(input$width)), na.rm = TRUE),
    height = function() max(c(100, abs(input$height)), na.rm = TRUE)
  )

  positionDf = reactive({   .debug("position")
    align = req(plotAlignment())
    scale = plotScaling()
    mat = cbind(x = align$xall,
                y = align$yall + scale$boxh/2,
                idInt = align$plotord)
    as.data.frame(mat)
  })

  # Single click: Text annotation -------------------------------------------

  observeEvent(input$ped_click, {   .debug("pedclick")
    posDf = positionDf()
    idInt = nearPoints(posDf, input$ped_click, xvar = "x", yvar = "y",
                       threshold = 20, maxpoints = 1)$idInt |> req()
    id = labels(pedigree$ped)[idInt]
    currSel = sel()
    newSel = if(id %in% currSel) .mysetdiff(currSel, id) else c(currSel, id)
    sel(newSel)
  })

  # Double click: Text annotation -------------------------------------------

  # Temp copy of textAnnot, to be used in the module.
  # Could use textAnnot directly, but want to go via pedUpdate (stack etc)
  textAnnotTemp = reactiveVal()

  observeEvent(input$ped_dblclick, {   .debug("dblclick")
    posDf = positionDf()
    idInt = nearPoints(posDf, input$ped_dblclick, xvar = "x", yvar = "y",
                       threshold = 20, maxpoints = 1)$idInt |> req()
    id = labels(pedigree$ped)[idInt]
    isolate(textAnnotTemp(textAnnot()))
    showAnnotationModal(input, output, session, id, textAnnotTemp)
  })

  observeEvent(textAnnotTemp(), { .debug("textAnnotTemp")
    updatePed(textAnnot = textAnnotTemp())})

  # Save --------------------------------------------------------------------

  output$savePed = downloadHandler(
    filename = "quickped.ped",
    content = function(con) {
      inclHead = "head" %in% input$include
      inclFamid = "famid" %in% input$include
      inclAff = "aff" %in% input$include

      ped = pedigree$ped
      df = as.data.frame(ped)
      if(inclFamid)
        df = cbind(famid = 1, df)
      if(inclAff) {
        aff = union(styles$hatched, names(styles$fill))
        df = cbind(df, aff = ifelse(labels(ped) %in% aff, 2, 1))
      }

      write.table(df, file = con, col.names = inclHead, row.names = FALSE,
                  quote = FALSE, sep = "\t", fileEncoding = "UTF-8")
    }
  )

  output$savePlotPng = downloadHandler(
    filename = "quickped.png",
    content = function(con)
      tryCatch({
        align = plotAlignment()
        scale = plotScaling()
        annot = plotAnnotation()

        png(con, width = input$width, height = input$height)
        drawPed(align, annotation = annot, scaling = scale)
        dev.off()
      },
      error = errModal),
    contentType = "image/png"
  )

  output$savePlotPdf = downloadHandler(
    filename = "quickped.pdf",
    content = function(file)
      tryCatch({
        align = plotAlignment()
        scale = plotScaling()
        annot = plotAnnotation()

        cairo_pdf(file, width = input$width/72, height = input$height/72)
        drawPed(align, annotation = annot, scaling = scale)
        dev.off()
      },
      error = errModal),
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

  observeEvent(input$coeffTable, {   .debug("coeffTable")
    if(1 %in% pedigree$twins$code) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    ped = pedigree$ped
    ids = sel()
    if(!length(ids))
      ids = labels(ped)
    updateTextInput(session, "coeffIds", value = toString(sortIds(ped, ids)))
    tryCatch(showModal(dlg), error = errModal)
  })


  # Action when clicking "Download" in the modal dialog
  output$saveCoeffTable = downloadHandler(
    filename = "quickped-relatedness.txt",
    content = function(file) {
      idsTxt = req(input$coeffIds)

      tryCatch({
        ids = trimws(strsplit(idsTxt, ",")[[1]])
        coeff = input$coeffSelect # coefficients selected
        self = "self" %in% input$coeffInclude

        if(!length(chr <- input$coeffChrom))
          stop2("Please select chromosome type")
        Xchrom = if(length(chr) == 2) NA else chr == "X"

        # Compute table
        tab = coeffTable(pedigree$ped, ids, coeff = coeff, Xchrom = Xchrom, self = self)

        write.table(tab, file = file, col.names = TRUE, row.names = FALSE,
                    quote = FALSE, sep = "\t", fileEncoding = "UTF-8")
        removeModal()
      },
      error = errModal)
    }
  )

  ### Triangle plot

  observeEvent(input$triangle, {   .debug("triangle")
    if(1 %in% pedigree$twins$code) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    ped = pedigree$ped
    ids = sel()
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
    plotKappa(pedigree$ped, ids = sel(), mode = input$trianglemode, pedArrows = "arrows" %in% input$settings),
    width = 560, height = 480,
    res = 72 # too low, but increasing it disturbs everything else
  )

  output$saveTriangle = downloadHandler(
    filename = "triangle.png",
    content = function(file) {
      png(file, width = 560*2, height = 480*2, res = 72*2)
      plotKappa(pedigree$ped, ids = sel(), mode = input$trianglemode, pedArrows = "arrows" %in% input$settings)
      dev.off()
    },
    contentType = "image/png"
  )

  observeEvent(input$describe, {   .debug("describe")
    ped = req(pedigree$ped)
    if(1 %in% pedigree$twins$code) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    ids = sortIds(ped, ids = sel())

    if(length(ids) != 2) {
      relText("Please select exactly 2 individuals.")
      return()
    }

    paths = verbalisr::verbalise(ped, ids)
    txt = format(paths)
    txt = gsub("([[:graph:]])  ([[:graph:]])", "\\1 \\2", txt) # remove double spaces
    relText(txt)
  })

  observeEvent(input$coeffs, {   .debug("coeffs")
    if(1 %in% pedigree$twins$code) {
      relText("This feature does not support MZ twin pedigrees.")
      return()
    }

    ped = req(pedigree$ped)
    ids = sortIds(ped, ids = sel())
    N = length(ids)

    if(!N %in% 1:2) {
      relText(c("Please select 1 or 2 individuals.",
                "(Or click the Table button for more options.)"))
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

  # Generate R code----------------------------------------------------------

  codeTxt = reactiveVal(NULL)

  # Render in modal dialog, created with createCodeModal when pressing rcode button (see below)
  output$showCode = renderText(req(codeTxt()))

  # observeEvent(input$copyCode, {
  #   if (!is.null(codeTxt())) {
  #     utils::writeClipboard(codeTxt())
  #   }
  # })

  output$saveCode = downloadHandler(
    filename = "quickped.R",
    content = function(con) {
      cat(codeTxt(), file = con)
      removeModal()
    },
    contentType = "text/plain"
  )

  observeEvent(input$rcode, {   .debug("R code")
    code = generateCode(ped = req(pedigree$ped),
                        twins = pedigree$twins,
                        miscarriage = pedigree$miscarriage,
                        styles = reactiveValuesToList(styles),
                        textAnnot = textAnnot(),
                        cex = input$cex,
                        symbolsize = input$symbolsize,
                        margins = input$mar,
                        width = input$width,
                        height = input$height)

    codeTxt(code)
    showModal(createCodeModal())
  })

}


shinyApp(ui = ui, server = server)
