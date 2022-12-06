library(tidyverse)
library(data.table)
library(plotly)
library(janitor)
library(RColorBrewer)

Abates <- fread("./Abates.csv") 
Abates

Abates1 <- select(Abates, Matadouro, MAC, Tipo_abate, Exploracao, Raca) %>%  
  mutate(Matadouro = (str_replace(Matadouro,c("<",">"),""))) %>%
  mutate(Raca = (str_replace(Raca,c("<",">"),"")))

# Grafico da percentagem de tipo de abate
graph_tipoabate <- plot_ly(data = count(Abates1, Tipo_abate), 
                      labels = ~Tipo_abate, values = ~n, type = "pie") %>% 
              layout(title = "Tipo de Abates")

graph_tipoabate
# # # # # # #

# Grafico de barras do total de abates por matadouro
abates_matadouro <- summarize(
    group_by(Abates1, Matadouro), 
    count=n()
  )

graph_abates_matadouro <- abates_matadouro %>% 
  plot_ly(x = ~count, y = ~Matadouro, type = 'bar') %>% 
  layout(title = "Abates por Matadouro")

graph_abates_matadouro
# # # # # # # # # #

# Grafico tipo de abate por matadouro
tipoabate_matadouro <- tabyl(Abates1, Matadouro, Tipo_abate)

graph_tipoabate_matadouro <- tipoabate_matadouro %>% 
  plot_ly(x = ~E, y = ~Matadouro, type = 'bar', name = 'Emergencia') %>% 
  add_trace(x = ~N, name = 'Normal') %>% 
  add_trace(x = ~S, name = 'Sanitario') %>% 
  layout(title = "Tipo de Abate por Matadouro", barmode = 'group')

graph_tipoabate_matadouro

# Grafico abates por matadouro por raca

# abates_matadouro_raca <- tabyl(Abates1, Matadouro, Raca)
# tabela_count_raca <- left_join(abates_matadouro, abates_matadouro_raca)

tab_raca_matadouro <- group_by(Abates1, Matadouro) %>% count(Raca)

# tab_raca_matadouro <- tab_raca_matadouro %>% mutate(Raca = (str_replace(Raca,c("<",">"),"")))

# Criar palete para as nossas 38 racas, porque as paletes pre definidas nao suportam tantas classes
pal <- brewer.pal(12, "Set3") 
pal <- colorRampPalette(pal)(38)

graph_abates_matadouro_raca <- plot_ly(data = tab_raca_matadouro, x = ~Matadouro, y = ~n,
                                       color = ~Raca, colors = pal,
                                       type = 'scatter', mode = 'markers')

graph_abates_matadouro_raca

# First try ANOVA -> acho que vamos cagar nisto 
# model <- aov(Raca ~ Tipo_abate, data = Abates1)
 
# Chi square independence test

tab_raca_tipoabate <- tabyl(Abates1, Raca, Tipo_abate)

chisq_raca_tipoabate <- chisq.test(tab_raca_tipoabate)
chisq_raca_tipoabate
# Temos aviso de aproximacao incorreta e isso tem a ver com freq inferior a 5(?)

# Para contornar podemos usar o Fisher exact test

fisher_raca_tipoabate <- fisher.test(tab_raca_tipoabate)
# Error in stats::fisher.test(., ...) : FEXACT error 40. Out of workspace. 
# Nao encontro respostas a isto online

# Eploracao com o tipo de abate

tab_exp_tipoabate <- tabyl(Abates1, Exploracao, Tipo_abate)

chisq_exp_tipoabate <- chisq.test(tab_exp_tipoabate)
chisq_exp_tipoabate
