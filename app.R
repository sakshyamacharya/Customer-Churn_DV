library(shiny)
library(plotly)
library(dplyr)

# Load the dataset directly and ensure no additional spaces in column names
df <- read.csv("Customer_Churn_Updated.csv")

# Define User Interface
ui <- fluidPage(
  titlePanel("Interactive Data Visualizations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Choose a plot type:",
                  choices = c("Histogram of Call Failures", "Box Plot of Customer Value",
                              "Heatmap of SMS Frequency", "Scatter Plot of Minutes vs. Customer Value",
                              "Line Plot of Subscription Length"))
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  output$plot <- renderPlotly({
    # Use the correct column names with periods or underscores as provided
    plot <- switch(input$plotType,
                   "Histogram of Call Failures" = plot_ly(df, x = ~`Call..Failure`, type = "histogram", 
                                                          name = "Call Failures", 
                                                          marker = list(color = 'rgba(100, 150, 238, 0.7)')) %>%
                     layout(title = "Distribution of Call Failures",
                            xaxis = list(title = "Call Failures"),
                            yaxis = list(title = "Frequency")),
                   "Box Plot of Customer Value" = plot_ly(df, y = ~`Customer.Value`, type = "box",
                                                          name = "Customer Value", 
                                                          marker = list(color = 'rgba(255, 182, 193, 0.5)')) %>%
                     layout(title = "Customer Value Distribution",
                            xaxis = list(title = ""),
                            yaxis = list(title = "Customer Value")),
                   "Heatmap of SMS Frequency" = plot_ly(df, x = ~`Frequency.of.SMS`, y = ~`Distinct.Called.Numbers`, z = ~`Age.Group`, type = 'heatmap', colors = colorRamp(c("blue", "green"))) %>%
                     layout(title = "SMS Frequency vs. Distinct Called Numbers by Age Group",
                            xaxis = list(title = "Frequency of SMS"),
                            yaxis = list(title = "Distinct Called Numbers")),
                   "Scatter Plot of Minutes vs. Customer Value" = plot_ly(df, x = ~Minutes_of_Use, y = ~`Customer.Value`, type = "scatter", mode = "markers",
                                                                          name = "Minutes vs. Value") %>%
                     layout(title = "Minutes of Use vs. Customer Value",
                            xaxis = list(title = "Minutes of Use"),
                            yaxis = list(title = "Customer Value")),
                   "Line Plot of Subscription Length" = plot_ly(df, x = ~`Age.Group`, y = ~`Subscription..Length`, type = "scatter", mode = "lines+markers",
                                                                name = "Subscription Length", 
                                                                line = list(color = 'rgba(255, 99, 71, 0.6)')) %>%
                     layout(title = "Subscription Length by Age Group",
                            xaxis = list(title = "Age Group"),
                            yaxis = list(title = "Subscription Length"))
    )
    
    # Apply a consistent color scheme
    plot %>%
      layout(plot_bgcolor = "#e5ecf6", paper_bgcolor = "#e5ecf6", font = list(color = "#2c3e50"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

