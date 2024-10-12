#load libraries for shiny
library(shiny)
library(shinydashboard)
library(DT)
#load data manipulation/plotting libraries
library(ggplot2)
library(quarto)
library(jsonlite)
#-------------------------------------------------------------------------------
################################UI CODE#########################################
#We want Five pages: Start, where the process is described; Data Upload, where you can upload csv files; Plotting, where you can plot data uploaded and select variables to do so; Export, where you can choose what it is you want to export. 
#we will explicitly not give the app the ability to bookmark stuff, as we want to purge everything after use.

header <- dashboardHeader(title = "ShinyExams")
  
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Start", tabName = "start", icon = icon("chart-bar")),
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Update Data", tabName = "update", icon=icon("edit")),
      menuItem("Plotting", tabName = "plotting", icon = icon("chart-bar")),
      menuItem("Export", tabName = "export", icon = icon("download")))
)
#-----------
#TAB LAYOUTS
#Set the layout of the start tab
startlayout <- tabItem(tabName = "start",
                       fluidRow(
                         column(width = 12,
                          h2("Welcome to ShinyExams"),
                          p("This app lets you easily upload test data and do basic operations with it through a point-and-click interface."),
                          p("There are several steps you need to take to make sure that your data is processed correctly by this app."),
                          tags$ol(
                            tags$li("Firstly, upload a csv file with your data into the application in the ",
                              tags$b("Upload Data"),"menu."),
                            tags$li("Then, you need to check whether factor variables (anything categorical like gender etc.) is indeed coded as factor, this is displayed in the overview."),
                            tags$li("If you need to change data types or names of variables, you can do so in the ", 
                                    tags$b("Update Data"), "menu tab."),
                            tags$li("Once this is done, check back at the Upload Data tab whether it worked as intended."),
                            tags$li("Once this is done, you can proceed plotting or exporting the data in the respective menu tabs..")
                       ),
                       p("This app was written and designed by ",tags$i("Oliver D. Reithmaier"),". If you have a suggestion for future development of the app, find a bug or just want to say thanks, you can do so on github: ", tags$a(href='https://github.com/k4tana/ShinyExams',"Visit the linked page"), "to star, favorite, or comment!")
                       )
                       )
                       )

#Set the layout of the upload tab
uploadlayout <- tabItem(tabName = "upload",
                        fluidRow(
                          column(width = 12,
                            h2("Data Upload"),
                            p("Here you can select any", tags$i("CSV-File")," to upload. After uploading, you will see a summary of each column below."))),
                        fluidRow(
                          box(width = 4, title = "Select Data to Upload", solidHeader = T, status = "primary",
                                             fileInput('file1', 'Choose CSV File', 
                                                       accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')))),
                        fluidRow(
                          column(width=12,
                                 conditionalPanel("output.fileUploaded==false", 
                                          box(title = "Descriptive Results", 
                                              status = "success",
                                                     div("No data uploaded yet"))),
                          ),column(width = 12,
                                 conditionalPanel("output.fileUploaded==true", 
                                                  box(title = "Descriptive Results",
                                                      status = "success",
                                                      DTOutput("variable_info")))
                        ))
)
#Set the layout of the update tap
updatelayout <- tabItem(tabName = "update",
                        fluidRow(
                          column(width = 12,
                          h2("Update the Data"),
                          p("On this page, you can change data types of variables, and their respective column names in the data set."))),
                        fluidRow(
                          box(title = "Change Column Names", uiOutput("column_names_ui"), width = 6, status = "info"),
                          box(title="Change Data Types", uiOutput("column_types_ui"), width=6, status = "info")
                        ),
                        fluidRow(
                          column(width = 4,
                            actionButton("update_names_types","Update Column Names & Types")
                          )
                        )
)
#Set the layout of the plot tab
plotlayout <- tabItem(tabName = "plotting",
                      fluidRow(
                        column(width=12,
                        h2("Plots"),
                        p("On this page, you can plot variables. You can choose between boxplots, density plots, and histograms."))),
                      fluidRow(
                        box(title = "Plot", width = 8, status = "success",plotOutput("plot")),
                        box(title="Variable Selection", width=4, status = "info", 
                            selectInput("xcol", "X Variable", choices = NULL),
                            selectInput("plot_type", "Plot Type", choices = c("Boxplot", "Density Plot", "Histogram")), 
                            selectInput("fillcol", "Separator Variable", choices = NULL, selected = NULL))
                        )
                      )
#Set the layout of the export tab
exportlayout <- tabItem(tabName = "export",
                        fluidRow(
                          column(width = 12,
                          h2("Data Export"),
                          p("This page lets you select different analysis formats to download in a ", tags$i("PDF-File"),". Here's how you do this:",tags$ol(tags$li("Specify the desired output in the ", tags$i("Select Output Type-"), "Window and then select parameters in the field next to it."), tags$li("Click ", tags$i("Add new item to list-"),"Button, after which you will see an overview of your selections in the", tags$i("Data to Export-"), "field."),tags$li("Export the analysis results by clicking on the Export Button")))
                        )),
                        fluidRow(
                          column(width = 4,
                                 selectInput("desire_outcome", "Select Output Type", choices = c("Statistics", "Plot"), selected = NULL)),
                          column(width = 4,
                                 conditionalPanel("input.desire_outcome == 'Plot'", 
                                                  selectInput("xcol_output","X Variable", choices = NULL),
                                                  selectInput("plot_type_output", "Plot Type", choices = c("Boxplot", "Density Plot", "Histogram"), selected = NULL),
                                                  selectInput("fillcol_output", "Separator Variable", choices = NULL, selected = NULL)),
                                 conditionalPanel("input.desire_outcome == 'Statistics'",
                                                  selectInput("stats_var", "Variable for Statistic", choices = NULL, selected = NULL), 
                                                  checkboxGroupInput("stats", "Included Statistics", choices = c("Mean", "SD", "Min", "Max", "N"), selected = c("Mean", "SD", "Min", "Max", "N")))
                        ),
                        column(width = 4)),
                        fluidRow(
                          column(width = 12,
                                 actionButton("add_item", "Add new item to export list")),
                          p("")
                        ),
                        fluidRow(
                          column(width = 12,
                                 p(""))),
                        fluidRow(
                          column(width=12,
                                 box(title = "Data to Export", status = "success", DTOutput("selected_items_ui"))
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                                 p(""),
                                 downloadButton('export_pdf','Export Selected Results to PDF')
                          )
                        )
                        )

#Compose the body from the individual layouts
body <- dashboardBody(tabItems(
  startlayout,
  uploadlayout,
  updatelayout,
  plotlayout,
  exportlayout
))

#Compose the UI from the different parts
ui <- dashboardPage(header,sidebar,body)

#-------------------------------------------------------------------------------
########################SERVER CODE#############################################
#-------------------------------------------------------------------------------
server <- function(input, output, session){
  # FILE IMPORT
  data <- reactive({
    if(is.null(input$file1)){
      return(NULL)
    }
    req(input$file1)
    # Read the file
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    df
  })
  output$fileUploaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  # INITIALIZE REACTIVE VAL FOR UPDATED DATA
  updated_data <- reactiveVal()
  
  # EVENT LISTENER TO CATCH DATA CHANGES AND SET LABELS IN THE UPDATE DATA TAB AND EXPORT TAB ACCORDINGLY
  observeEvent(data(), {
    df <- data()
    updated_data(df)
    num_vars <- c("none", names(df)[sapply(df, is.numeric)])
    factor_vars <- c("none", names(df)[sapply(df, is.factor)])
    updateSelectInput(session, "xcol", choices = num_vars)
    updateSelectInput(session, "fillcol", choices = factor_vars)
    updateSelectInput(session, "xcol_output", choices= num_vars)
    updateSelectInput(session, "fillcol_output", choices= factor_vars)
    updateSelectInput(session, "stats_var", choices = num_vars)
  })
  # REACTIVE TO DISCERN "NOT YET UPLOADED" AND "UPLOADED" VIEWS
  
  #Eventlistener to catch changes to data.
  observe({
    if(is.null(data)){
      output$variable_info <- renderUI({
        p("No data uploaded yet")
      })
    }else{
  # OUTPUT FUNCTION: DISPLAY THE DATA OVERVIEW IN THE UPLOAD TAB
  output$variable_info <- renderDT({
    df <- updated_data()
    req(df)
    
    info <- lapply(names(df), function(col) {
      if (is.numeric(df[[col]])) {
        stats <- list(
          DataType = "Numeric",
          Mean = round(mean(df[[col]], na.rm = TRUE), digits = 2),
          SD = round(sd(df[[col]], na.rm = TRUE), digits = 2),
          NAs = sum(is.na(df[[col]]))
        )
      } else if (is.factor(df[[col]])) {
        stats <- list(
          DataType = "Factor",
          Mean = "NaN",
          SD = "NaN",
          NAs = sum(is.na(df[[col]]))
        )
      } else if (is.character(df[[col]])) {
        stats <- list(
          DataType = "Character",
          Mean = "NaN",
          SD = "NaN",
          NAs = sum(is.na(df[[col]]))
        )
      } else {
        stats <- list(type = "Unknown")
      }
      c(Variable = col, unlist(stats))
    })
    # Create a data frame from the list of lists (info)
    info_df <- do.call(rbind, info)
    
    # Cast the dataframe into the render function, options make sure the list can be scrolled through vertically default -1 makes sure all df rows are shown.
    datatable(data.frame(info_df, stringsAsFactors = FALSE), 
              options = list(scrollY = "515px",
                             lengthMenu = list(c(-1, 10, 25, 50, 100), 
                                               c("All", "10", "25", "50", "100"))))
  })}})
  
  # OUTPUT FUNCTION: REACTIVE UI TO ACCOMMODATE NEW COLUMN NAMES
  output$column_names_ui <- renderUI({
    req(input$file1)
    df <- data()
    lapply(names(df), function(col) {
      textInput(paste0("col_", col), paste("Rename Column:", col), value = col)
    })})
  
  # OUTPUT FUNCTION: REACTIVE UI TO ACCOMMODATE NEW COLUMN TYPES
  output$column_types_ui <- renderUI({
    req(input$file1)
    df <- data()
    lapply(names(df), function(col) {
      current_type <- if (is.numeric(df[[col]])) {
        "numeric"
      } else if (is.factor(df[[col]])) {
        "factor"
      } else if (is.character(df[[col]])) {
        "character"
      } else {
        "unknown"
      }
      selectInput(paste0("type_", col), 
                  paste("Set Data Type:", col), 
                  choices = c("numeric", "factor", "character"), 
                  selected = current_type)
    })
  })
  
  # EVENT LISTENER THAT CHECKS FOR DATA TYPE UPDATES
  observeEvent(input$update_names_types, {
    df <- data()
    req(df)
    
    columns <- names(df)
    rename_map <- setNames(sapply(columns, function(col) {
      input[[paste0("col_", col)]]
    }), columns)
    names(df) <- rename_map
    
    for (col in names(df)) {
      new_type <- input[[paste0("type_", col)]]
      current_type <- if (is.numeric(df[[col]])) {
        "numeric"
      } else if (is.factor(df[[col]])) {
        "factor"
      } else if (is.character(df[[col]])) {
        "character"
      } else {
        "unknown"
      }
      
      if (new_type != current_type) {
        if (new_type == "numeric") {
          df[[col]] <- as.numeric(df[[col]])
        } else if (new_type == "factor") {
          df[[col]] <- as.factor(df[[col]])
        } else if (new_type == "character") {
          df[[col]] <- as.character(df[[col]])
        }
      }
    }
    
    updated_data(df)
    #check whether we need this!
    num_vars <- c("none", names(df)[sapply(df, is.numeric)])
    factor_vars <- c("none", names(df)[sapply(df, is.factor)])
    updateSelectInput(session, "xcol", choices = num_vars)
    updateSelectInput(session, "fillcol", choices = factor_vars)
    updateSelectInput(session, "xcol_output", choices = num_vars)
    updateSelectInput(session, "fillcol_output", choices = factor_vars)
    updateSelectInput(session, "stats_var", choices = num_vars)
  })
  
  # OUTPUT FUNCTION: PLOT RENDERER FOR PLOT OUTPUTS
  output$plot <- renderPlot({
    req(input$xcol, input$plot_type)
    df <- updated_data()
    req(df)
    
    if(input$xcol!="none"){
    p <- ggplot(df, aes_string(x = input$xcol)) +
      theme(panel.grid.major = element_blank(), 
            panel.background = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size = 15),
            text = element_text(size = 15))
    
    if (input$plot_type == "Boxplot" && input$fillcol != "none") {
      p <- p + aes_string(fill = input$fillcol) +
        scale_fill_manual(values = c("#59B4AB", "#D7B365"))
    } else if (input$fillcol != "none") {
      p <- p + facet_wrap(as.formula(paste("~", input$fillcol)))
    }
    
    if (input$plot_type == "Boxplot") {
      p <- p + geom_boxplot() +
        theme(axis.ticks.y = element_blank(), 
              axis.text.y = element_blank())
    } else if (input$plot_type == "Density Plot") {
      p <- p + geom_density()
    } else if (input$plot_type == "Histogram") {
      p <- p + geom_histogram()
    }
    print(p)
    }
  })
  
  # Reactive Value for the export list
  export_items <- reactiveVal(data.frame())

  # EVENT LISTENER THAT FILLS A LIST WHICH USES STATISTICS AND PLOTS. 
  observeEvent(input$add_item, {
    items <- export_items()
    if(is.null(items)==T){
      items <- data.frame()
    }
    i <- length(items) + 1
    helper <- data.frame()
      if(input$desire_outcome=="Plot"){
        helper<- rbind(c(input$xcol_output, input$plot_type_output, input$fillcol_output,NA))
      }else if(input$desire_outcome=="Statistics"){
        helper <- rbind(c(input$stats_var, NA,NA,paste(input$stats, collapse = ',')))
      }
    items <- rbind(items,helper)
    export_items(items)
  })

  # OUTPUT: RENDERS THE LIST THAT THE USER HAS SELECTED AND HANDLES
  output$selected_items_ui <- renderDT({
    items <- export_items()
    #This ensures that it doesn't do anything wonky unless there's data in the table.
    if(length(items==4)){
      colnames(items) <- c("Variable", "Plot Type", "Separator Variable", "Exported Statistics")
    }
    datatable(items)
  })
  
  # OUTPUT FUNCTION THAT HANDLES EXPORT TO PDF
  output$export_pdf <- downloadHandler(
    filename = "ShinyExams-Report.pdf",
    content = function(file) {
      report_path <- tempfile(fileext = ".qmd")
      file.copy("report.qmd", report_path, overwrite = TRUE)
      params <- list(df=updated_data(),
                     wishlist= export_items())
      quarto_render(input = "report.qmd",
                    execute_params = params,
                    output_format= "pdf")
    }
  )
}
#----- Launch Command.
shinyApp(ui=ui, server=server)

