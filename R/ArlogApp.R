#' Launch surveillance dashboard
#'
#' @param data Optional pre-processed data frame. If NULL, will show file upload interface
#' @importFrom shiny shinyApp fluidPage sidebarLayout fileInput dateRangeInput selectInput
#' @export
run_surveillance_dashboard <- function(data = NULL) {
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(lubridate)

  ui <- fluidPage(
    style = "width: 100vw; max-width: 100vw;",
    tags$style(HTML("
      body {
        max-width: 100%;
        width: 100%;
        padding: 0;
        margin: 0;
      }
      .container-fluid {
        padding: 0;
        margin: 0;
        width: 100%;
        max-width: 100%;
      }
    ")),
    titlePanel("Healthcare-Acquired Infections Dashboard"),
    fluidRow(
      div(style="display: flex; gap: 10px; margin-left: 15px;",
          div(style="flex: 1; max-width: 300px;",
              dateRangeInput("date_range", "Select Date Range:",
                             start = NULL,
                             end = NULL)
          ),
          div(style="flex: 1; max-width: 200px;",
              selectInput("organism", "Select Organism:",
                          choices = NULL)
          ),
          div(style="flex: 1; max-width: 200px;",
              selectInput("facility", "Select Facility:",
                          choices = NULL)
          )
      )
    ),
    mainPanel(
      style = "width: 100%; max-width: 100%; margin: 0; padding: 0;",
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(6, plotOutput("cases_over_time")),
                   column(6, plotOutput("organism_distribution"))
                 ),
                 fluidRow(
                   column(12, plotOutput("resistance_patterns"))
                 ),
                 fluidRow(
                   column(6, plotOutput("sample_source_dist")),
                   column(6, plotOutput("turnaround_time"))
                 ),
                 fluidRow(
                   column(6, plotOutput("age_distribution")),
                   column(6, plotOutput("visit_type_comparison"))
                 )
        ),
        tabPanel("Detailed Data",
                 DTOutput("data_table")
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Reactive data source that can handle both pre-loaded and uploaded data
    data_source <- reactive({
      if (!is.null(data)) {
        return(data)
      }

      req(input$file)
      process_surveillance_data(input$file$datapath)
    })

    # Initialize UI elements once data is available
    observe({
      req(data_source())
      df <- data_source()

      # Update date range input
      updateDateRangeInput(session, "date_range",
                           start = min(df$date_of_collection, na.rm = TRUE),
                           end = max(df$date_of_collection, na.rm = TRUE))

      # Update select inputs
      updateSelectInput(session, "organism",
                        choices = c("All", levels(df$organism)))

      updateSelectInput(session, "facility",
                        choices = c("All", levels(df$facility_of_origin)))
    })

    # Filter data based on inputs
    filtered_data <- reactive({
      req(data_source())
      df <- data_source()

      if (!is.null(input$organism) && input$organism != "All") {
        df <- df %>% filter(organism == input$organism)
      }

      if (!is.null(input$facility) && input$facility != "All") {
        df <- df %>% filter(facility_of_origin == input$facility)
      }

      if (!is.null(input$date_range)) {
        df <- df %>%
          filter(date_of_collection >= input$date_range[1],
                 date_of_collection <= input$date_range[2])
      }

      df
    })

    # Cases over time plot
    output$cases_over_time <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        count(date_of_collection) %>%
        ggplot(aes(x = date_of_collection, y = n)) +
        geom_line() +
        geom_smooth(method = "loess", se = TRUE) +
        labs(title = "Cases Over Time",
             x = "Date",
             y = "Number of Cases") +
        theme_minimal(base_size = 14)
    })

    # Organism distribution plot
    output$organism_distribution <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        count(organism) %>%
        ggplot(aes(x = reorder(organism, n), y = n)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Distribution of Organisms",
             x = "Organism",
             y = "Number of Cases") +
        theme_minimal(base_size = 14)
    })

    # Resistance patterns plot
    output$resistance_patterns <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        filter(!is.na(mechanism_submitters_report)) %>%
        count(organism, mechanism_submitters_report) %>%
        ggplot(aes(x = reorder(mechanism_submitters_report, n), y = n, fill = organism)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Resistance Mechanisms",
             x = "Mechanism",
             y = "Number of Cases") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
    })

    # Sample source distribution
    output$sample_source_dist <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        count(source) %>%
        ggplot(aes(x = reorder(source, n), y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Sample Sources",
             x = "Source",
             y = "Number of Cases") +
        theme_minimal(base_size = 14)
    })

    # Turnaround time analysis
    output$turnaround_time <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        mutate(turnaround_days = as.numeric(date_reported - date_received)) %>%
        filter(!is.na(turnaround_days)) %>%
        ggplot(aes(x = turnaround_days)) +
        geom_histogram(binwidth = 1, fill = "steelblue") +
        labs(title = "Test Result Turnaround Time",
             x = "Days",
             y = "Count") +
        theme_minimal(base_size = 14)
    })

    # Age distribution
    output$age_distribution <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        mutate(age = as.numeric(date_of_collection - date_of_birth)/365.25) %>%
        filter(!is.na(age)) %>%
        ggplot(aes(x = age)) +
        geom_histogram(binwidth = 5, fill = "steelblue") +
        labs(title = "Age Distribution of Cases",
             x = "Age (years)",
             y = "Count") +
        theme_minimal(base_size = 14)
    })

    # Visit type comparison
    output$visit_type_comparison <- renderPlot({
      req(filtered_data())
      filtered_data() %>%
        count(screening_or_clinical) %>%
        ggplot(aes(x = screening_or_clinical, y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Cases by Visit Type",
             x = "Visit Type",
             y = "Count") +
        theme_minimal(base_size = 14)
    })

    # Detailed data table
    output$data_table <- renderDT({
      req(filtered_data())
      filtered_data() %>%
        select(-patient_name) %>%  # Exclude patient name for privacy
        datatable(options = list(pageLength = 10,
                                 scrollX = TRUE,
                                 scrollY = "500px"))
    })
  }

  shinyApp(ui = ui, server = server)
}
