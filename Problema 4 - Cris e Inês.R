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
