# Load necessary libraries
library(shiny)
library(DT)
library(ggplot2)
library(plotly)

# Load necessary libraries
library(tidyverse)
library(lubridate)


# Create dummy data for descriptive statistics table
descriptive_stats_dummy <- data.frame(
  Group = c("Normal", "Outlier"),
  Count = c(150, 50), # Example counts
  `Count %` = c(75, 25), # Example percentages
  Avg_Prediction = c(0.3, 0.7), # Example average prediction scores
  Avg_TP = c(150, 200), # Example average true positives
  Avg_TN = c(150, 100) # Example average true negatives
  # ... Add other averages as needed ...
)



# Define top predictors
top_10_predictors <- c('TD013', 'MB007', 'TD009', 'TD005', 'AP003', 'TD014', 'AP001', 'CR019', 'CR009', 'TD010')

# Create dummy data for two years
set.seed(123) # For reproducibility
dummy_data <- expand.grid(
  month = month(seq(ymd('2021-01-01'), ymd('2022-12-31'), by = '1 month')),
  year = year(seq(ymd('2021-01-01'), ymd('2022-12-31'), by = '1 month'))
) %>%
  mutate(
    predictions = runif(n(), 0, 1), # Random predictions between 0 and 1
    tp = sample(100:200, n(), replace = TRUE), # True Positives
    tn = sample(100:200, n(), replace = TRUE), # True Negatives
    fn = sample(0:100, n(), replace = TRUE),  # False Negatives
    fp = sample(0:100, n(), replace = TRUE)   # False Positives
  )

# Add dummy data for top predictors
for (predictor in top_10_predictors) {
  dummy_data[[predictor]] <- runif(nrow(dummy_data), 0, 1)
}

# Create a date column in the dummy_data to combine 'year' and 'month'
dummy_data <- dummy_data %>%
  mutate(Date = as.Date(paste(year, month, "1", sep = "-")))

# Expand the dummy data to include categories for each predictor
set.seed(123)  # For reproducibility

# Assuming each predictor has 3 categories
categories <- c("Low", "Medium", "High")

predictor_data <- expand.grid(
  month = seq(as.Date('2021-01-01'), by = 'month', length.out = 24),
  predictor = c('TD013', 'MB007', 'TD009', 'TD005'),
  category = categories
)

predictor_data$value <- runif(nrow(predictor_data), 0, 100)  # Random values as an example

# Add a binary indicator for anomaly in predictions (1 for anomaly, 0 for normal)
threshold <- 0.8
dummy_data$IsAnomaly <- ifelse(dummy_data$predictions > threshold, 1, 0)


# UI definition
ui <- fluidPage(
  titlePanel("ML Model Monitoring for Mortgage Default Prediction"),
  
  tabsetPanel(
    tabPanel("Model Performance", 
             selectInput("time_frame", "Select Year:", choices = c("2021", "2022")),
             plotlyOutput("confusionMatrixPlot"),
             DT::dataTableOutput("descriptiveStatTable")),
    tabPanel("Predictor Analysis", 
             fluidRow(
               column(6, plotlyOutput("plot_TD013")),
               column(6, plotlyOutput("plot_MB007"))
             ),
             fluidRow(
               column(6, plotlyOutput("plot_TD009")),
               column(6, plotlyOutput("plot_TD005"))
             )),
    tabPanel("Predictions Over Time", 
             selectInput("year_filter", "Select Year for Prediction Trends:", choices = c("2021", "2022")),
             plotlyOutput("predictionPlot"))
  )
)



# Server logic
server <- function(input, output) {
  
  # Confusion Matrix Monthly Visualization
  output$confusionMatrixPlot <- renderPlotly({
    req(input$time_frame) # Require input to proceed
    # Filter data based on selected year and prepare it for plotting
    filtered_data <- dummy_data %>%
      filter(year == as.numeric(input$time_frame)) %>%
      group_by(month) %>%
      summarise(
        TP = sum(tp),
        TN = sum(tn),
        FP = sum(fp),
        FN = sum(fn)
      ) %>%
      pivot_longer(cols = c(TP, TN, FP, FN), names_to = "variable", values_to = "value")
    
    # Create line plot
    plot_ly(filtered_data, x = ~month, y = ~value, color = ~variable, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Confusion Matrix",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Count"),
             hovermode = 'closest')
  })
  

  # Descriptive Statistics Table
  output$descriptiveStatTable <- DT::renderDataTable({
    DT::datatable(descriptive_stats_dummy)
  })
  
  # Predictor Analysis Plot
  # Function to create plot for a given predictor
  create_predictor_plot <- function(predictor_name) {
    renderPlotly({
      filtered_data <- predictor_data %>%
        filter(predictor == predictor_name) %>%
        group_by(month, category) %>%
        summarise(value = sum(value), .groups = 'drop') %>%
        mutate(month = format(month, "%Y-%m")) # Formatting for consistent x-axis
      
      plot_ly(filtered_data, x = ~month, y = ~value, color = ~category, type = 'bar', name = ~category) %>%
        layout(barmode = 'stack', 
               xaxis = list(title = "Month", tickangle = 45),
               yaxis = list(title = "Value"),
               title = paste("Predictor Analysis -", predictor_name))
    })
  }
  
  # Create a plot for each predictor
  output$plot_TD013 <- create_predictor_plot("TD013")
  output$plot_MB007 <- create_predictor_plot("MB007")
  output$plot_TD009 <- create_predictor_plot("TD009")
  output$plot_TD005 <- create_predictor_plot("TD005")
  
  # Predictions Over Time Plot
  # Predictions Over Time Plot
  output$predictionPlot <- renderPlotly({
    req(input$year_filter) # Require the year filter input to proceed
    
    # Filter data based on selected year and calculate monthly anomaly counts
    anomaly_data <- dummy_data %>%
      filter(year(Date) == as.numeric(input$year_filter)) %>%
      group_by(Month = floor_date(Date, "month")) %>%
      summarise(AnomalyCount = sum(IsAnomaly)) %>%
      ungroup()
    
    # Create line plot for anomaly trend
    plot_ly(anomaly_data, x = ~Month, y = ~AnomalyCount, type = 'scatter', mode = 'lines+markers',
            line = list(shape = "spline"), # Smooth line
            name = 'Anomaly Trend') %>%
      layout(title = "Anomaly Predictions Over Time",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Anomalies"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

