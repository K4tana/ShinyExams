library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Exams"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Update Data", tabName = "rename_update", icon = icon("edit")),
      menuItem("Plotting", tabName = "plotting", icon = icon("chart-bar")),
      menuItem("Summary", tabName = "summary", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload",
        fluidRow(
          column(width = 12, h2("Welcome to Shiny Exams")),
          column(width = 12, p("This page allows you to do easy analyses of your exam data. It uses R under the hood to process and analyze the data. To start, you can upload your data as a csv file and gain some easy insights on this page.")),
          column(width = 12, fileInput('file1', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
          column(width = 12, DTOutput("variable_info"))
        )
      ),
      tabItem(
        tabName = "rename_update",
        fluidRow(
          column(width = 12, h2("Data Updates")),
          column(width = 12, p("On this page, you can change data types of variables, and their respective column names in the data set.")),
          column(width = 6, uiOutput("column_names_ui")),
          column(width = 6, uiOutput("column_types_ui")),
          column(width = 12, actionButton("update_names_types", "Update Column Names & Types"))
        )
      ),
      tabItem(
        tabName = "plotting",
        fluidRow(
          column(width = 12, h2("Plots of Variables")),
          column(width = 12, p("On this page, you can plot variables. You can choose between boxplots, density plots, and histograms.")),
          column(width = 6, selectInput("xcol", "X Variable", choices = NULL)),
          column(width = 6, selectInput("plot_type", "Plot Type", choices = c("Boxplot", "Density Plot", "Histogram"))),
          column(width = 6, selectInput("fillcol", "Separator Variable", choices = NULL, selected = "none")),
          column(width = 12, plotOutput("plot"))
        )
      ),
      tabItem(
        tabName = "summary",
        fluidRow(
          column(width = 12, h2("Exporting Results")),
          column(width = 12, p("This page lets you select the results to export as a summary PDF.")),
          column(width = 12, actionButton("add_item", "Add New Item to List")),
          column(width = 12, uiOutput("selected_items_ui")),
          column(width = 12, downloadButton('export_pdf', 'Compile and Export to PDF'))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    df
  })
  
  updated_data <- reactiveVal(NULL)
  
  observeEvent(data(), {
    df <- data()
    updated_data(df)
    factor_vars <- c("none", names(df)[sapply(df, is.factor)])
    updateSelectInput(session, "xcol", choices = names(df))
    updateSelectInput(session, "fillcol", choices = factor_vars)
  })
  
  output$variable_info <- renderDT({
    df <- updated_data()
    req(df)
    
    info <- lapply(names(df), function(col) {
      if (is.numeric(df[[col]])) {
        stats <- list(
          type = "Numeric",
          mean = round(mean(df[[col]], na.rm = TRUE), digits = 2),
          sd = round(sd(df[[col]], na.rm = TRUE), digits = 2),
          NAs = sum(is.na(df[[col]]))
        )
      } else if (is.factor(df[[col]])) {
        stats <- list(
          type = "Factor",
          mean = "NaN",
          sd = "NaN",
          NAs = sum(is.na(df[[col]]))
        )
      } else if (is.character(df[[col]])) {
        stats <- list(
          type = "Character",
          mean = "NaN",
          sd = "NaN",
          NAs = sum(is.na(df[[col]]))
        )
      } else {
        stats <- list(type = "Unknown")
      }
      c(Variable = col, unlist(stats))
    })
    
    info_df <- do.call(rbind, info)
    datatable(data.frame(t(info_df), stringsAsFactors = FALSE), options = list(scrollX = TRUE))
  })
  
  output$column_names_ui <- renderUI({
    req(input$file1)
    df <- data()
    lapply(names(df), function(col) {
      textInput(paste0("col_", col), paste("Rename Column:", col), value = col)
    })
  })
  
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
      
      selectInput(paste0("type_", col), paste("Set Data Type:", col), choices = c("numeric", "factor", "character"), selected = current_type)
    })
  })
  
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
    factor_vars <- c("none", names(df)[sapply(df, is.factor)])
    updateSelectInput(session, "xcol", choices = names(df))
    updateSelectInput(session, "fillcol", choices = factor_vars)
  })
  
  output$plot <- renderPlot({
    req(input$xcol, input$plot_type)
    df <- updated_data()
    req(df)
    
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
      p <- p + geom_boxplot()
    } else if (input$plot_type == "Density Plot") {
      p <- p + geom_density()
    } else if (input$plot_type == "Histogram") {
      p <- p + geom_histogram()
    }
    
    if(input$plot_type == "Boxplot") {
      p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
    }
    
    print(p)
  })
  
  export_items <- reactiveVal(list())
  
  observeEvent(input$add_item, {
    items <- export_items()
    item_id <- as.character(length(items) + 1)
    items[[item_id]] <- list(type = NULL, xcol = NULL, plot_type = NULL, fillcol = NULL, stats = NULL, stats_var = NULL)
    for (previous_item_id in names(items)) {
      if (!is.null(input[[paste0("type_", previous_item_id)]])) {
        items[[previous_item_id]]$type <- input[[paste0("type_", previous_item_id)]]
        items[[previous_item_id]]$xcol <- input[[paste0("xcol_", previous_item_id)]]
        items[[previous_item_id]]$plot_type <- input[[paste0("plot_type_", previous_item_id)]]
        items[[previous_item_id]]$fillcol <- input[[paste0("fillcol_", previous_item_id)]]
        items[[previous_item_id]]$stats_var <- input[[paste0("stats_var_", previous_item_id)]]
        items[[previous_item_id]]$stats <- input[[paste0("stats_", previous_item_id)]]
      }
    }
    export_items(items)
  })
  
  observe({
    items <- export_items()
    updated_df <- updated_data()
    for (item_id in names(items)) {
      observeEvent(input[[paste0("type_", item_id)]], {
        if (input[[paste0("type_", item_id)]] == "Statistics") {
          updateCheckboxGroupInput(session, paste0("stats_", item_id), 
                                   selected = c("mean", "sd", "min", "max", "n_observations"))
          items <- export_items()
          items[[item_id]]$stats <- c("mean", "sd", "min", "max", "n_observations")
          export_items(items)
        }
      }, ignoreInit = TRUE, ignoreNULL = FALSE)
      
      updateSelectInput(session, paste0("xcol_", item_id), choices = names(updated_df), selected = items[[item_id]]$xcol)
      updateSelectInput(session, paste0("plot_type_", item_id), selected = items[[item_id]]$plot_type)
      updateSelectInput(session, paste0("fillcol_", item_id), choices = c("none", names(updated_df)[sapply(updated_df, is.factor)]), selected = items[[item_id]]$fillcol)
      updateSelectInput(session, paste0("stats_var_", item_id), choices = names(updated_df), selected = items[[item_id]]$stats_var)
      updateCheckboxGroupInput(session, paste0("stats_", item_id), selected = items[[item_id]]$stats)
    }
  })
  
  output$selected_items_ui <- renderUI({
    items <- export_items()
    updated_df <- updated_data()
    ui_list <- lapply(names(items), function(item_id) {
      fluidRow(
        column(width = 4, selectInput(paste0("type_", item_id), "Item Type", choices = c("Plot", "Statistics"), selected = items[[item_id]]$type)),
        column(width = 4, conditionalPanel(
          condition = sprintf("input.type_%s == 'Plot'", item_id),
          selectInput(paste0("xcol_", item_id), "X Variable", choices = names(updated_df), selected = items[[item_id]]$xcol),
          selectInput(paste0("plot_type_", item_id), "Plot Type", choices = c("Boxplot", "Density Plot", "Histogram"), selected = items[[item_id]]$plot_type),
          selectInput(paste0("fillcol_", item_id), "Separator Variable", choices = c("none", names(updated_df)[sapply(updated_df, is.factor)]), selected = items[[item_id]]$fillcol)
        )),
        column(width = 4, conditionalPanel(
          condition = sprintf("input.type_%s == 'Statistics'", item_id),
          selectInput(paste0("stats_var_", item_id), "Variable for Stats", choices = names(updated_df), selected = items[[item_id]]$stats_var),
          checkboxGroupInput(paste0("stats_", item_id), "Statistics to include", choices = c("mean", "sd", "min", "max", "n_observations"), selected = items[[item_id]]$stats)
        ))
      )
    })
    do.call(tagList, ui_list)
  })
  
  output$export_pdf <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      items <- export_items()
      params <- list(dataset = updated_data(), items = items)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)