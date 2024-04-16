#This code helps in creation of a simple dashboard using R Shiny
# R-Shiny-Dashboard
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(lubridate)
library(readr)

# Load and clean the dataset
data_cleaned <- read_csv("listings.csv") %>%
  distinct(id, .keep_all = TRUE) %>% # Remove duplicates
  filter(!is.na(review_scores_rating), review_scores_rating > 50) %>% # Handle missing values and filter
  mutate(
    price = as.numeric(gsub("[^0-9.]", "", price)), # Ensure price is numeric
    first_review = as.Date(first_review, format = "%Y-%m-%d") # Convert first_review to Date
  )

# UI Definition
ui <- fluidPage(
  titlePanel("Airbnb Listings Insights"),
  
  # Sidebar for filters
  sidebarLayout(
    sidebarPanel(
      selectInput("cancellationPolicy", "Cancellation Policy",
                  choices = c("All", unique(data_cleaned$cancellation_policy))),
      sliderInput("priceRange", "Price Range",
                  min = min(data_cleaned$price, na.rm = TRUE),
                  max = max(data_cleaned$price, na.rm = TRUE),
                  value = c(min(data_cleaned$price, na.rm = TRUE), max(data_cleaned$price, na.rm = TRUE))),
      selectInput("selectedYear", "Select Year",
                  choices = c("All", unique(format(data_cleaned$first_review, "%Y"))))
    ),
    
    # Main panel for visualizations
    mainPanel(
      tabsetPanel(
        tabPanel("Review Score Distribution", plotOutput("reviewScoreHist")),
        tabPanel("Map Visualization", leafletOutput("listingsMap")),
        tabPanel("Seasonal Trends", plotlyOutput("reviewTrends"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    data <- data_cleaned %>%
      filter((cancellation_policy == input$cancellationPolicy | input$cancellationPolicy == "All"),
             price >= input$priceRange[1] & price <= input$priceRange[2],
             format(first_review, "%Y") == input$selectedYear | input$selectedYear == "All")
    data
  })
  
  # Review Score Histogram
  output$reviewScoreHist <- renderPlot({
    ggplot(filteredData(), aes(x = review_scores_rating)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = "Distribution of Review Scores", x = "Review Score", y = "Count")
  })
  
  # Map Visualization
  output$listingsMap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~name)
  })
  
  # Seasonal Trends Visualization
  output$reviewTrends <- renderPlotly({
    data <- filteredData() %>%
      mutate(month = floor_date(first_review, "month")) %>%
      group_by(month) %>%
      summarize(reviewsCount = n()) %>%
      ungroup()
    
    plot_ly(data, x = ~month, y = ~reviewsCount, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'blue', width = 2),
            marker = list(color = 'red', size = 7)) %>%
      layout(title = "Review Trends",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Reviews"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
