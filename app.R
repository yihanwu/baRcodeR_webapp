library(shiny)
library(baRcodeR)

if (!dir.exists("www")) dir.create("www")

simple_barcode_tab_ui <- function(id){
    ns <- shiny::NS(id)
    # simple label tab
    shiny::tabPanel(
        title = "Simple ID Code Generation", 
        value = "simple_labs", 
        icon = shiny::icon("bars"),
        shiny::fillRow(
            shiny::fillCol(shiny::tagList(# user input elements
                shiny::tags$h1("Simple ID Codes", id = "title"),
                shiny::textInput(ns("prefix"), 
                                 "ID String", 
                                 value = "", 
                                 width=NULL, 
                                 placeholder="Type in ... ..."),
                shiny::numericInput(ns("start_number"), 
                                    "From (integer)", 
                                    value = NULL, 
                                    min = 1, 
                                    max = Inf, 
                                    width=NULL),
                shiny::numericInput(ns("end_number"), 
                                    "To (integer)", 
                                    value = NULL, 
                                    min = 1, 
                                    max = Inf, 
                                    width=NULL),
                shiny::numericInput(ns("digits"), 
                                    "digits", 
                                    value = 3, 
                                    min = 1, 
                                    max = Inf, 
                                    width=NULL),
                # textOutput("check"),
                # shiny::actionButton(ns("make_csv"), "Create Label.csv"),
                shiny::downloadButton(ns("download_csv"), "Download Label.csv"),
                shiny::p("Note: CSV file will be downloaded directly. All generated and uploaded files will be deleted at the end of the session (ie when the browser is closed.)"))),
            shiny::fillRow(shiny::tagList(# output code snippet for reproducibility
                shiny::tags$h3("Reproducible code"),
                shiny::verbatimTextOutput(ns("label_code")),
                # output showing label preview
                shiny::tags$h3("Preview"),
                DT::DTOutput(ns("label_df")))
                
            )
            
        )
    )
}

simple_barcode_server <- function(input, output, session){
    Labels <- shiny::reactive({
        shiny::validate(
            shiny::need(input$prefix != "", "Please enter a prefix"),
            shiny::need(input$start_number != "", "Please enter a starting value"),
            shiny::need(input$end_number != "", "Please enter an ending value"),
            shiny::need(input$digits != "", "Please enter the number of digits")
        )
        baRcodeR::uniqID_maker(user=FALSE, 
                               string = input$prefix, 
                               level = seq(input$start_number, input$end_number), 
                               digits = input$digits)
    })
    # preview of simple labels
    output$label_df<-DT::renderDataTable(Labels())
    # writing simple labels to temp file
    fileName_react <- reactiveValues()
    shiny::observe({
        Labels <- Labels()
        dir.create(file.path("www", session$token))
        fileName_react$fileName <- tempfile(
            file.path(session$token, sprintf("Labels_%s", Sys.Date())), 
            tmpdir = "www", fileext= ".csv")
        utils::write.csv(Labels, file = fileName_react$fileName, row.names=FALSE)
        print(fileName_react$filename)
        
    })
    
    #make file available for download
    output$download_csv <- shiny::downloadHandler(
        filename = function(){
            sprintf("Labels_%s.csv", Sys.Date())
        },
        content = function(file){
            file.copy(as.character(fileName_react$fileName), file)
        },
        contentType = "text/csv"
        
    )
    
    # preview of generated labels
    output$label_code<-shiny::renderPrint(
        noquote(paste0(
            "uniqID_maker(user = FALSE, string = \'", input$prefix, 
            "\', ", "level = c(", input$start_number, ",", input$end_number, 
            "), digits = ", input$digits, ")")))
    
}

hier_barcode_tab_ui <- function(id){
    ns <- shiny::NS(id)
    # hierarchy label tab
   shiny::tabPanel(
       title = "Hierarchical ID Code Generation", 
       value = "hier_labs", 
       icon = shiny::icon("sitemap"),
       shiny::fillRow(
           shiny::fillCol(shiny::tagList(
               # ui elements
               shiny::tags$h1("Hierarchical ID Codes", id = "title"),
               shiny::numericInput(ns("hier_digits"), 
                                   "digits", 
                                   value = 2, 
                                   min = 1, max = Inf, width=NULL),
               shiny::textInput(ns("hier_prefix"), 
                                "ID String", 
                                value = "", 
                                width=NULL, 
                                placeholder="Type in ... ..."),
               shiny::numericInput(ns("hier_start_number"), 
                                   "From (integer)", 
                                   value = NULL, 
                                   min = 1, max = Inf, width=NULL),
               shiny::numericInput(ns("hier_end_number"), 
                                   "To (integer)", 
                                   value = NULL, 
                                   min = 1, max = Inf, width=NULL),
               shiny::actionButton(ns('insertBtn'), 'Add level'),
               shiny::actionButton(ns('removeBtn'), 'Remove level'),
               # shiny::actionButton("hier_label_preview", "Preview Labels"),
               # shiny::actionButton(ns("hier_label_make"), "Create Labels.csv"),
               shiny::downloadButton(ns("download_csv"), "Download Label.csv"),
               shiny::p("Note: CSV file will be downloaded directly. All generated and uploaded files will be deleted at the end of the session (ie when the browser is closed.)")
           )),
           shiny::fillCol(shiny::tagList(
               # code snippet
               shiny::tags$h3("Reproducible Code"),
               shiny::verbatimTextOutput(ns("hier_code")),
               # output elements
               shiny::tags$h3("Hierarchy"),
               # output hierarchy as df
               shiny::verbatimTextOutput(ns("list_check")),
               shiny::tags$h3("Label Preview"),
               # label preview
               DT::DTOutput(ns("hier_label_df"))))
       )
        
           
       )
}

hier_barcode_server <- function(input, output, session){
    # server-side for hierarchical values
    # set reactiveValues to store the level inputs
    values<-shiny::reactiveValues()
    # set up data frame within reactiveValue function
    values$df<-data.frame(Prefix = character(0), start=integer(), end=integer(), stringsAsFactors = FALSE)
    # delete row from the df if button is pressed.
    shiny::observeEvent(input$removeBtn, {
        shiny::isolate(values$df<-values$df[-(nrow(values$df)),])
    })
    # add level to df
    shiny::observeEvent(input$insertBtn, {
        # level_name<-input$insertBtn
        shiny::validate(
            shiny::need(input$hier_prefix != "", "Please enter a prefix"),
            shiny::need(input$hier_start_number != "", "Please enter a starting value"),
            shiny::need(input$hier_end_number != "", "Please enter an ending value"),
            shiny::need(input$hier_digits != "", "Please enter the number of digits")
        )
        shiny::isolate(values$df[nrow(values$df) + 1,]<-c(input$hier_prefix, input$hier_start_number, input$hier_end_number))
        shiny::updateTextInput(session = session, 
                               "hier_prefix", 
                               "Label String", 
                               value = character(0))
        shiny::updateNumericInput(session = session, 
                                  "hier_start_number", 
                                  "From (integer)", 
                                  value = numeric(0), 
                                  min = 1, max = Inf)
        shiny::updateNumericInput(session = session, 
                                  "hier_end_number", 
                                  "To (integer)", 
                                  value = numeric(0), 
                                  min = 1, max = Inf)
    })
    # make hierarchical labels
    hier_label_df<-shiny::reactive({
        # shiny::validate(
        #   shiny::need(input$hier_prefix != "", "Please enter a prefix"),
        #   shiny::need(input$hier_start_number != "", "Please enter a starting value"),
        #   shiny::need(input$hier_end_number != "", "Please enter an ending value"),
        #   shiny::need(input$hier_digits != "", "Please enter the number of digits")
        # )
        shiny::validate(
            shiny::need(nrow(values$df) > 1, "Please add a level")
        )
        hierarchy <- split(values$df, seq(nrow(values$df)))
        hier_Labels <- baRcodeR::uniqID_hier_maker(user=FALSE, hierarchy = hierarchy, end = NULL, digits = input$hier_digits)
        hier_Labels
    })
    hier_code_snippet_obj<-shiny::reactive({
        begin_string<-noquote(strsplit(paste(split(values$df, seq(nrow(values$df))), collapse=', '), ' ')[[1]])
        replace_string<-gsub(pattern = "list\\(", replacement = "c\\(", begin_string)
        replace_string<-paste(replace_string, sep="", collapse="")
        noquote(paste0(
            "uniqID_hier_maker(user = FALSE, hierarchy = list(", replace_string, 
            "), end = NULL, digits = ", input$hier_digits, ")"))
        
    })
    output$hier_code<-shiny::renderText(hier_code_snippet_obj())
    # rough df of the level df
    output$list_check<-shiny::renderPrint(values$df)
    # preview of hierarchical labels
    output$hier_label_df<-DT::renderDataTable(hier_label_df())
    # writing labels to temp file
    fileName_react <- reactiveValues()
    shiny::observe({
        Labels <- hier_label_df()
        dir.create(file.path("www", session$token))
        fileName_react$fileName <- tempfile(
            file.path(session$token, sprintf("Labels_%s", Sys.Date())), 
            tmpdir = "www", fileext= ".csv")
        utils::write.csv(Labels, file = fileName_react$fileName, row.names=FALSE)
        print(fileName_react$filename)
        
    })
    
    #make file available for download
    output$download_csv <- shiny::downloadHandler(
        filename = function(){
            sprintf("Labels_%s.csv", Sys.Date())
        },
        content = function(file){
            file.copy(as.character(fileName_react$fileName), file)
        },
        contentType = "text/csv"
        
    )
}

barcode_gen_tab_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Barcode Creation", 
                    value= "bar_gen", 
                    icon = shiny::icon("qrcode"),
                    shiny::fillRow(
                        shiny::fillCol(
                            shiny::tagList(
                                shiny::tags$h1("Generate Barcode PDF"),
                                shiny::tags$p("All uploaded and generated files will be deleted at the end of the session (ie when the browser is closed). "),
                                shiny::fileInput(ns("labels"), 
                                                 "Upload Label Text File", 
                                                 multiple=FALSE,
                                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                shiny::checkboxInput(ns("header"), "Header in file?", value=TRUE),
                                shiny::actionButton(ns("label_check"), "Import ID Code File"),
                                shiny::tags$h3("(Optional) PDF Layout Options"),
                                shiny::textInput(ns("filename"), 
                                                 "PDF file name", 
                                                 value = "LabelsOut"),
                                shiny::selectInput(inputId = ns("err_corr"), 
                                                   label = "Error Correction", 
                                                   choices = c("L (up to 7% damage)"="L", 
                                                               "M (up to 15% damage)"= "M",
                                                               "Q (up to 25% damage)" = "Q", 
                                                               "H (up to 30% damage)" = "H"), 
                                                   multiple=FALSE),
                                shiny::selectInput(ns("type"), 
                                                   "Barcode Type", 
                                                   choices = list("Matrix (2D)" = "matrix",
                                                                  "Linear (1D)" = "linear"),
                                                   multiple = FALSE),
                                shiny::numericInput(ns("font_size"), 
                                                    "Font Size", 
                                                    value = 12, min = 2, max = 100),
                                shiny::radioButtons(ns("across"), 
                                                    "Print across?", 
                                                    choices = c(Yes = TRUE, No = FALSE),
                                                    selected = TRUE),
                                shiny::numericInput(ns("erow"), 
                                                    "# of rows to skip", 
                                                    value = 0, min = 0, max = 20, width=NULL),
                                shiny::numericInput(ns("ecol"), 
                                                    "# of columns to skip", 
                                                    value = 0, min = 0, max = 20, width=NULL),
                                shiny::checkboxInput(ns("trunc"), 
                                                     "Truncate label text?", 
                                                     value=FALSE),
                                shiny::numericInput(ns("numrow"), 
                                                    "Number of label rows on sheet", 
                                                    value = 20, min = 1, max = 100, width=NULL, step = 1),
                                shiny::numericInput(ns("numcol"), 
                                                    "Number of label columns on sheet", 
                                                    value = 4, min = 1, max = 100, width=NULL, step = 1),
                                shiny::numericInput(ns("page_width"), 
                                                    "Page Width (in)", 
                                                    value = 8.5, min = 1, max = 20, width=NULL,
                                                    step = 0.5),
                                shiny::numericInput(ns("page_height"), 
                                                    "Page Height (in)", 
                                                    value = 11, 
                                                    min = 1, max = 20, width=NULL, step = 0.5),
                                shiny::numericInput(ns("width_margin"), 
                                                    "Width margin of page (in)", 
                                                    value = 0.25, 
                                                    min = 0, max = 20, width=NULL, step = 0.05),
                                shiny::numericInput(ns("height_margin"), 
                                                    "Height margin of page (in)", 
                                                    value = 0.5, min = 0, max = 20, width=NULL, step = 0.05),
                                shiny::numericInput(ns("label_width"), 
                                                    "Width of label (in)", 
                                                    value = NA, min=0, max=100),
                                shiny::numericInput(ns("label_height"), 
                                                    "Height of label (in)", 
                                                    value = NA, min=0, max=100),
                                shiny::numericInput(ns("x_space"), 
                                                    "Horizontal space between barcode and text",
                                                    value = 0, min = 0, max = 1),
                                shiny::numericInput(ns("y_space"), 
                                                    "Vertical location of text on label", 
                                                    value = 0.5, min = 0, max = 1)
                            )
                        ),
                        shiny::fillCol(
                            shiny::tagList(# shiny::tags$h3("Import text file"),
                                           # output elements
                                           shiny::tags$h3("Preview"),
                                           shiny::plotOutput(ns("label_preview"), height = "auto", width = "auto"),
                                           # label preview datatable
                                           DT::DTOutput(ns("check_make_labels")),
                                           # code snippet
                                           shiny::tags$h3("Reproducible Code"),
                                           shiny::verbatimTextOutput(ns("PDF_code_render")),
                                           shiny::tags$h3(""),
                                           shiny::actionButton(ns("make_pdf"), "Make PDF"),
                                           # status of pdf making
                                           shiny::textOutput(ns("PDF_status")),
                                           shiny::downloadButton(ns("download_pdf_output"), "Download PDF")
                        )
                    )
                    )

    )
}

barcode_gen_server <- function(input, output, session){
    # pdf making server side
    # check label file
    Labels_pdf<-shiny::eventReactive(input$label_check, {
        shiny::req(input$labels)
        Labels<-utils::read.csv(input$labels$datapath, header=input$header, stringsAsFactors = FALSE)
        Labels
    })
    # preview label file
    output$check_make_labels<-DT::renderDataTable(Labels_pdf(), 
                                                  server = FALSE, 
                                                  selection = list(mode = "single", target = "column", selected = 1))
    # label preview image
    output$label_preview <- shiny::renderImage({
        if(input$type == "matrix") {
            code_vp <- grid::viewport(
                x=grid::unit(0.05, "npc"), 
                y=grid::unit(0.8, "npc"), 
                width = grid::unit(0.3 * (input$page_width - 2 * input$width_margin)/input$numcol, "in"), 
                height = grid::unit(0.6 * (input$page_height - 2 * input$height_margin)/input$numrow, "in"), 
                just=c("left", "top"))
            
            label_vp <- grid::viewport(
                x=grid::unit((0.4 + 0.6 * input$x_space)* (input$page_width - 2 * input$width_margin)/input$numcol, "in"), 
                y=grid::unit(input$y_space, "npc"), width = grid::unit(0.4, "npc"), 
                height = grid::unit(0.8, "npc"), 
                just=c("left", "center"))
            
            label_plot <- qrcode_make(Labels = Labels_pdf()[1, input$check_make_labels_columns_selected], 
                                      ErrCorr = input$err_corr)
            
        } else {
            code_vp <- grid::viewport(x=grid::unit(0.05, "npc"), 
                                      y=grid::unit(0.8, "npc"), 
                                      width = grid::unit(0.9 * (input$page_width - 2 * input$width_margin)/input$numcol, "in"), 
                                      height = grid::unit(0.8 * (input$page_height - 2 * input$height_margin)/input$numrow, "in"), 
                                      just=c("left", "top"))
            # text_height <- ifelse(input$Fsz / 72 > (input$page_height - 2 * input$height_margin)/input$numrow * 0.3, (input$page_height - 2 * input$height_margin)/input$numrow * 0.3, input$Fsz/72)
            
            label_vp <- grid::viewport(
                x=grid::unit(0.5, "npc"), 
                y = grid::unit(1, "npc"), 
                width = grid::unit(1, "npc"), 
                height = grid::unit((input$page_height - 2 * input$height_margin)/input$numrow * 0.3, "in"), 
                just = c("centre", "top"))
            
            label_plot <- code_128_make(Labels = Labels_pdf()[1, input$check_make_labels_columns_selected])
        }
        outputfile <- tempfile(fileext=".png")
        grDevices::png(outputfile, 
                       width = (input$page_width - 2 * input$width_margin)/input$numcol, 
                       (input$page_height - 2 * input$height_margin)/input$numrow, 
                       units = "in", res=100)
        # grid::grid.rect()
        grid::pushViewport(code_vp)
        grid::grid.draw(label_plot)
        grid::popViewport()
        grid::pushViewport(label_vp)
        grid::grid.text(label = Labels_pdf()[1, input$check_make_labels_columns_selected], 
                        gp = grid::gpar(fontsize = input$font_size, lineheight = 0.8))
        grDevices::dev.off()
        list(src = outputfile,
             width = 80 * (input$page_width - 2 * input$width_margin)/input$numcol, 
             height = 80 * (input$page_height - 2 * input$height_margin)/input$numrow,
             alt = "Label Preview")
    }, deleteFile = TRUE
    )
    # text indicator that pdf finished making
    PDF_done <- reactiveVal("Generating barcodes may take a few seconds. Wait until 'Download available' appears.")
    
    fileName_react <- reactiveValues()
    
    shiny::observeEvent(input$make_pdf,{
        PDF_done("Generating PDF.")
        dir.create(file.path("www", session$token))
        fileName_react$fileName <- tempfile(
            file.path(session$token, input$filename), 
            tmpdir = "www", fileext= "")
        print(fileName_react$fileName)
        baRcodeR::custom_create_PDF(
            user=FALSE, 
            Labels = Labels_pdf()[, input$check_make_labels_columns_selected], 
            name = fileName_react$fileName, 
            type = input$type, 
            ErrCorr = input$err_corr, 
            Fsz = input$font_size, 
            Across = input$across, 
            ERows = input$erow, 
            ECols = input$ecol, 
            trunc = input$trunc, 
            numrow = input$numrow, 
            numcol = input$numcol, 
            page_width = input$page_width, 
            page_height = input$page_height, 
            height_margin = input$height_margin, 
            width_margin = input$width_margin, 
            label_width = input$label_width, 
            label_height = input$label_height, 
            x_space = input$x_space, 
            y_space = input$y_space)
        PDF_done("Download available.")
    })
    
    #make file available for download
    output$download_pdf_output <- shiny::downloadHandler(
        filename = function(){
            paste0(input$filename, ".pdf")
        },
        content = function(file){
            file.copy(paste0((fileName_react$fileName), ".pdf"), file)
        },
        contentType = "application/pdf"
        
    )
    
    PDF_code_snippet <- shiny::reactive({
        noquote(
            paste0("custom_create_PDF(user=FALSE, Labels = label_csv[,",input$check_make_labels_columns_selected, 
                   "], name = \'", paste0(input$filename, ".pdf"), "\', 
               type = \'", input$type, 
                   "\', ErrCorr = \'", input$err_corr, 
                   "\', Fsz = ", input$font_size, 
                   ", Across = ", input$across, 
                   ", ERows = ", input$erow, 
                   ", ECols = ", input$ecol, 
                   ", trunc = ", input$trunc, 
                   ", numrow = ", input$numrow, 
                   ", numcol = ", input$numcol, 
                   ", page_width = ", input$page_width, 
                   ", page_height = ", input$page_height, 
                   ", width_margin = ", input$width_margin, 
                   ", height_margin = ", input$height_margin, 
                   ", label_width = ", input$label_width, 
                   ", label_height = ", input$label_height, 
                   ", x_space = ", input$x_space, 
                   ", y_space = ", input$y_space, ")"))
    })
    csv_code_snippet<-shiny::reactive({noquote(
        paste0(
            "label_csv <- read.csv( \'", input$labels$name, 
            "\', header = ", input$header, 
            ", stringsAsFactors = FALSE)"))})
    output$PDF_code_render<-shiny::renderText({
        paste(csv_code_snippet(), PDF_code_snippet(), sep = "\n")
    })
    # rendering of pdf indicator
    output$PDF_status <- shiny::renderText(PDF_done())
}


ui <- shiny::navbarPage("baRcodeR",
                        simple_barcode_tab_ui("simple_labs"),
                        hier_barcode_tab_ui("hier_labs"),
                        barcode_gen_tab_ui("bar_gen"),
                        shiny::tabPanel("About",
                                        shiny::tags$h1("About this application:"),
                                        shiny::tags$p("This shiny application is a web-based version of the", shiny::tags$a("baRcodeR", href = "https://cran.r-project.org/web/packages/baRcodeR/index.html"), "RStudio add-on. Use this app to generate simple, and hierarchical labels and create printable PDF labels."),
                                        shiny::tags$p("The baRcodeR package was created by Robert Colautti and Yihan Wu and is part of the.", shiny::tags$a("rOpenSci project.", href = "https://ropensci.org/") ,"This web application and the package are maintained by Yihan Wu. For more information, see", shiny::tags$a("baRcodeR", href = "https://github.com/ropensci/baRcodeR")),
                                        shiny::tags$h2("Usage:"),
                                        shiny::tags$p("See", shiny::tags$a("documentation", href = "https://docs.ropensci.org/baRcodeR/articles/use-addin.html"), "for detailed instructions and screencaptures."),
                                        shiny::tags$p("Quick start for barcode creation: Select a text file of text labels to upload, import the file, click the 'make PDF' button to generate your PDF for download."),
                                        shiny::tags$h2("For problems:"),
                                        shiny::tags$p("Start an issue on the github repository for this", shiny::tags$a("shiny app (preferred option)", href="https://github.com/yihanwu/baRcodeR_webapp"), "or email the maintainer at yihan.wu@queensu.ca")))

server <- function(input, output, session) {
    shiny::callModule(simple_barcode_server, "simple_labs")
    shiny::callModule(hier_barcode_server, "hier_labs")
    shiny::callModule(barcode_gen_server, "bar_gen")
    onStop(function() unlink(file.path("www", session$token), recursive = TRUE))
}


shiny::shinyApp(ui = ui, server = server)
