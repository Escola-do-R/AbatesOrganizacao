library(tidyverse)
library(data.table)
library(lubridate)
library(freqtables)
library(plotly)

Abates <- fread("./Abates.csv") 

# 3 tipos diferentes de abates: Normal, Sanitário, Emergência
# Abates$MMA_CLA_CAR_GR corresponde a camada gordura
# Abates$MMA_CLA_CAR_CF corresponde a classificacao da carcaça na escala SEUROP
# Abates$MMA_CLA_CAR_CT corrresponde a classificação de acordo com idade e genero
# Regulamento 1308/2013 para verificar


# Temos de fazer se nao me engano a parte de Peso ao abate tendo em conta a raça, idade e sexo

# Assim, vou selecionar as colunas que me interessam e garantir que sao da classe certa
# Calculei idade ao abate usando funcao do lubridate %--% e arredondei a 1 casa decimal
# Peso convertido a numerico, necessario substituir as , por .
Abates_peso <- select(Abates, Data_abate, Data_nasc, Peso, Raca, Sexo) %>% 
  mutate(
    idade_ao_abate= round ((Data_nasc %--% Data_abate) / years(1),1),
    Peso = as.numeric(str_replace(Peso, ",", ".")),
    ) %>% 
  mutate(Raca = (str_replace(Raca,c("<",">"),"")))

# A nivel de estatistica descritiva por raça
Abates_prop_raca <- Abates_peso %>% 
  group_by(Raca) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3)) %>% 


Abates_prop_sexo <- Abates_peso %>% 
  group_by(Sexo) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

# Idade ao abate e variavel continua, primeiro separei em ranges e depois fiz freq table com esses ranges
Abates_peso$idade_range <- cut(Abates_peso$idade_ao_abate, breaks= c(0,1,2,3,4,5,10,20,30),
                               labels=c("0-1","1-2","2-3","3-4","4-5","5-10","10-20","+20"))

Abates_prop_idade <- Abates_peso %>% 
  group_by(idade_range) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

# Tabela freq 
Freq_abates_raca <- Abates_peso %>% 
  freq_table(Raca)

Freq_abates_sexo <- Abates_peso %>% 
  freq_table(Sexo)

Freq_abates_idade <- Abates_peso %>% 
  freq_table(idade_range)

# Representação gráfica

# abates_sexo <- summarize(
#   group_by(Abates_peso, Sexo), 
#   count=n()
# )
# 
graph_abates_sexo <- Abates_prop_sexo %>%
  plot_ly(x = ~n, y = ~Sexo, type = 'bar') %>%
  layout(title = "Abates por Sexo")
graph_abates_sexo

graph_abates_raca <- Abates_prop_raca %>% 
  plot_ly(x = ~n, y = ~Raca, type = 'bar') %>% 
  layout(title = "Abates por Raça")
graph_abates_raca

# Fiz um grafico de barras por range de idades, nao sei se tambem seria fixe fazer um histograma disto
graph_abates_idade <- Abates_prop_idade %>% 
  plot_ly(x=~n, y=~idade_range, type="bar")
graph_abates_idade
