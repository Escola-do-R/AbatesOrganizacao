library(tidyverse)
library(data.table)
library(plotly)

Abates <- fread("./Abates.csv") 
Abates

Abates1 <- select(Abates, Matadouro, MAC, Tipo_abate, Exploracao, Raca)

graph_tipo <- plot_ly(data = count(Abates1, Tipo_abate), 
                      labels = ~Tipo_abate, values = ~n, type = "pie") %>% 
              layout(title = "Tipo de Abates")

graph_tipo

abates_matadouro <- summarize(
    group_by(Abates1, Matadouro), 
    count=n()
  )

graph_abates_matadouro <- abates_matadouro %>% 
  plot_ly(x = ~count, y = ~Matadouro, type = 'bar') %>% 
  layout(title = "Abates por Matadouro")

graph_abates_matadouro
