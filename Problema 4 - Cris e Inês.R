library(tidyverse)
library(data.table)
library(plotly)
library(janitor)

Abates <- fread("./Abates.csv") 
Abates

Abates1 <- select(Abates, Matadouro, MAC, Tipo_abate, Exploracao, Raca) %>%  
  mutate(Matadouro = (str_replace(Matadouro,c("<",">"),"")))

graph_tipo <- plot_ly(data = count(Abates1, Tipo_abate), 
                      labels = ~Tipo_abate, values = ~n, type = "pie") %>% 
              layout(title = "Tipo de Abates")

graph_tipo


abates_matadouro <- summarize(
    group_by(Abates1, Matadouro), 
    count=n()
  )

tipoabate_matadouro <- tabyl(Abates1, Matadouro, Tipo_abate)
abate_raca_matadouro <- tabyl(Abates1, Matadouro, Raca)

graph_abates_matadouro <- abates_matadouro %>% 
  plot_ly(x = ~count, y = ~Matadouro, type = 'bar') %>% 
  layout(title = "Abates por Matadouro")

graph_abates_matadouro

abates_raca <- summarize(
  group_by(Abates1, Raca), 
  count=n()
)

