library(tidyverse)
library(data.table)
library(lubridate)
library(freqtables)
library(plotly)
library(rstatix)

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

# Inicio de estatistica descritiva
# Acho que repeti as tabelas de frequencia, mas if anything temos 2 maneiras possiveis de as fazer
# Fiz à brute force e com library freqtables

# Raca
Abates_prop_raca <- Abates_peso %>% 
  group_by(Raca) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

Freq_abates_raca <- Abates_peso %>% 
  freq_table(Raca)

stats_peso <- Abates_peso %>% 
  get_summary_stats(idade_ao_abate)

# Sexo
Abates_prop_sexo <- Abates_peso %>% 
  group_by(Sexo) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

Freq_abates_sexo <- Abates_peso %>% 
  freq_table(Sexo)

# Idade
# Idade ao abate e variavel continua, primeiro separei em ranges e depois fiz freq table com esses ranges
Abates_peso$idade_range <- cut(Abates_peso$idade_ao_abate, breaks= c(0,1,2,3,4,5,10,20,30),
                               labels=c("0-1","1-2","2-3","3-4","4-5","5-10","10-20","+20"))

Abates_prop_idade <- Abates_peso %>% 
  group_by(idade_range) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

Freq_abates_idade <- Abates_peso %>% 
  freq_table(idade_range)

# Representação gráfica
 
graph_abates_sexo <- Abates_prop_sexo %>%
  plot_ly(x = ~n, y = ~Sexo, type = 'bar') %>%
  layout(title = "Abates por Sexo") %>%
  layout(xaxis = list(title = "Frequência"), yaxis = list(title = "Sexo"))  
graph_abates_sexo

graph_abates_raca <- Abates_prop_raca %>% 
  plot_ly(x = ~n, y = ~Raca, type = 'bar') %>% 
  layout(title = "Abates por Raça") %>%
  layout(xaxis = list(title = "Frequência"), yaxis = list(title = "Raça"))  
graph_abates_raca

# Fiz um grafico de barras por range de idades (no plotly) e um histograma (simples com base R)
graph_abates_idade <- Abates_prop_idade %>% 
  plot_ly(x=~n, y=~idade_range, type="bar") %>%
  layout(title="Abates por Idade") %>%
  layout(xaxis = list(title = "Frequência"), yaxis = list(title = "Idade"))  
graph_abates_idade

# Acho que isto ficava mais bonito no plotly, mas o pc estava a empancar
histo_idade <- hist(Abates_peso$idade_ao_abate)

#a mesma coisa que o histo_idade mas no plotly (a mim nao me empancou?)
histo_idade_plotly <- Abates_peso %>% 
  plot_ly(x=~idade_range, type="histogram") %>%
  layout(title="Abates por Idade") %>%
  layout(xaxis = list(title = "Idade"), yaxis = list(title = "Frequência"))
histo_idade_plotly
