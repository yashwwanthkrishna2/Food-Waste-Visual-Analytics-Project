library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinyWidgets)
library(DT)

# Load data
data <- read.csv("C:\\Users\\yashw\\Desktop\\Final Project - Visual analytics\\Food Waste data and research - by country.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Global Food Waste Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Waste Analysis", tabName = "waste_analysis", icon = icon("globe")),
      menuItem("Waste Distribution", tabName = "distribution", icon = icon("th-list")),
      menuItem("Regional Overview", tabName = "regional_overview", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "waste_analysis",
              fluidRow(
                column(width = 12,
                       pickerInput("selectedCountries", "Select Countries:", choices = unique(data$Country), 
                                   multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE), selected = unique(data$Country)[1:10]),
                       style = "color: #000; font-weight: bold;"
                )
              ),
              plotlyOutput("wastePlot"),
              tabsetPanel(
                tabPanel("Household Waste", plotlyOutput("householdWastePlot")),
                tabPanel("Retail Waste", plotlyOutput("retailWastePlot")),
                tabPanel("Food Service Waste", plotlyOutput("foodServiceWastePlot"))
              )
      ),
      tabItem(tabName = "distribution",
              plotlyOutput("boxPlot")
      ),
      tabItem(tabName = "regional_overview",
              plotlyOutput("bubblePlot")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$wastePlot <- renderPlotly({
    filtered_data <- data %>% filter(Country %in% input$selectedCountries)
    p <- ggplot(filtered_data, aes(x = reorder(Country, combined.figures..kg.capita.year.), y = combined.figures..kg.capita.year.)) +
      geom_bar(stat = "identity", fill = "#3073AB") +
      theme_minimal() +
      labs(title = "Per Capita Food Waste by Country", x = "Country", y = "Waste (kg per capita per year)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$householdWastePlot <- renderPlotly({
    selected_data <- data %>%
      filter(Country %in% input$selectedCountries)
    p <- ggplot(selected_data, aes(x = reorder(Country, Household.estimate..kg.capita.year.), y = Household.estimate..kg.capita.year., fill = "Household")) +
      geom_bar(stat = "identity", fill = "#FF9F80") +
      labs(title = "Household Food Waste", x = "Country", y = "Waste (kg per capita per year)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    ggplotly(p)
  })
  
  output$retailWastePlot <- renderPlotly({
    selected_data <- data %>%
      filter(Country %in% input$selectedCountries)
    p <- ggplot(selected_data, aes(x = reorder(Country, Retail.estimate..kg.capita.year.), y = Retail.estimate..kg.capita.year., fill = "Retail")) +
      geom_bar(stat = "identity", fill = "#F8766D") +
      labs(title = "Retail Food Waste", x = "Country", y = "Waste (kg per capita per year)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    ggplotly(p)
  })
  
  output$foodServiceWastePlot <- renderPlotly({
    selected_data <- data %>%
      filter(Country %in% input$selectedCountries)
    p <- ggplot(selected_data, aes(x = reorder(Country, Food.service.estimate..kg.capita.year.), y = Food.service.estimate..kg.capita.year., fill = "Food Service")) +
      geom_bar(stat = "identity", fill = "#00BFC4") +
      labs(title = "Food Service Waste", x = "Country", y = "Waste (kg per capita per year)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    ggplotly(p)
  })
  
  output$boxPlot <- renderPlotly({
    p <- ggplot(data, aes(x = Region, y = combined.figures..kg.capita.year., fill = Region)) +
      geom_boxplot() +
      labs(title = "Distribution of Combined Waste Figures by Region", x = "Region", y = "Combined Waste (kg/capita/year)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "bottom") +
      scale_fill_brewer(palette = "Set3")
    ggplotly(p, tooltip = "y")
  })
  
  output$bubblePlot <- renderPlotly({
    regional_data <- data %>%
      group_by(Region) %>%
      summarise(TotalWaste = sum(combined.figures..kg.capita.year., na.rm = TRUE))
    plot_ly(regional_data, x = ~Region, y = ~TotalWaste,
            type = 'scatter', mode = 'markers',
            marker = list(size = ~sqrt(TotalWaste), color = ~TotalWaste, colorscale = 'Portland', showscale = TRUE),
            hoverinfo = 'text+x+y', text = ~paste("Region:", Region, "<br>Total Waste:", TotalWaste))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
