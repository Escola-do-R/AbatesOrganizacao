library(shiny)
library(maps)
library(mapproj)

source("./Problema 4 - Cris e Inês.R", encoding = "utf-8")

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
                              "Regional de Mafra"),
                  selected = "Santa Carnes")
    ),
    
    mainPanel(leafletOutput(outputId = "mapa"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$mapa <- renderLeaflet({
    switch(input$var, 
           "Santa Carnes" = SantaCarnes_leaflet,
           "Raporal" = Raporal_leaflet,
           "Regional de Mafra" = RegMafra_leaflet)
  })
}

# Run app ----
shinyApp(ui, server)