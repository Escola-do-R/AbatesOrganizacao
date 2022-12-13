library(shiny)
library(maps)
library(mapproj)

source("./helpers.R")

counties <- readRDS("./data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")

# User interface ----
ui <- fluidPage(
  titlePanel("Mapas Top3 Matadouros PT"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Representação geográfica da área de captação dos principais
               matadouros portugueses."),
      
      selectInput("var", 
                  label = "Escolha o matadouro pretendido:",
                  choices = c("Santa Carnes", "Raporal",
                              "Matadouro Regional de Mafra"),
                  selected = "Santa Carnes"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = Inf, value = c(0, Inf))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    percent_map(data, "darkgreen", input$var,
                 max = input$range[2], min = input$range[1])
  })
}

# Run app ----
shinyApp(ui, server)
