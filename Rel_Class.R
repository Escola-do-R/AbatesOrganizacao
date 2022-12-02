library(tidyverse)
library(data.table)
library(freqtables)
library(plotly)


# relacao entre raca e classificacao e raca e tipo de abate

abates<- fread("./Abates.csv")


abates1<- select(abates, Raca, Tipo_abate, MMA_CLA_CAR_CT,MMA_CLA_CAR_CF,MMA_CLA_CAR_GR) %>% 
  mutate(Raca = (str_replace(Raca,c("<",">"),""))) %>%
  mutate(across(c(MMA_CLA_CAR_CT,MMA_CLA_CAR_CF,MMA_CLA_CAR_GR), na_if, "NULL")) 
# utiliar na_if pq temos nulls naquelas colunas que nao nos informam de nada e estou a equiparar a na para lhe dar drop

### tabela de freq

Freq_raca <- freq_table(abates1, Raca)

Freq_Tipo <- freq_table(abates1, Tipo_abate)


Freq_CT <- freq_table(abates1, MMA_CLA_CAR_CT) %>%  drop_na()

Freq_CF <- freq_table(abates1, MMA_CLA_CAR_CF) %>%  drop_na()

Freq_GR <- freq_table(abates1, MMA_CLA_CAR_GR)%>%  drop_na()

# Tabelas contigencia

Raca_abate<- as.data.frame.matrix(table(abates1$Raca,abates1$Tipo_abate))%>%  drop_na()

Raca_CT<- as.data.frame.matrix(table(abates1$Raca,abates1$MMA_CLA_CAR_CT))%>%  drop_na()

Raca_CF<- as.data.frame.matrix(table(abates1$Raca,abates1$MMA_CLA_CAR_CF))%>%  drop_na()

Raca_GR<- as.data.frame.matrix(table(abates1$Raca,abates1$MMA_CLA_CAR_GR))%>%  drop_na()

#### Representacao grafica da estatistica descritiva ####

graph_abates_tipo <- Freq_Tipo %>%
  plot_ly(x = ~n, y = ~cat, type = 'bar') %>%
  layout(title = "Abates por Tipo") %>%
  layout(xaxis = list(title = "Frequencia"), yaxis = list(title = "Tipo")) 
# Este gradico nao serve para nada pq temos numeros infimos
graph_abates_class_CT <- Freq_CT %>%
  plot_ly(x = ~n, y = ~cat, type = 'bar') %>%
  layout(title = "Classificacao da carcaca") %>%
  layout(xaxis = list(title = "Frequencia"), yaxis = list(title = "Classificacao CT")) 
graph_abates_class_CT 

graph_abates_class_CF <- Freq_CF %>%
  plot_ly(x = ~n, y = ~cat, type = 'bar') %>%
  layout(title = "Classificacao da carcaca") %>%
  layout(xaxis = list(title = "Frequencia"), yaxis = list(title = "Classificacao CF"))
graph_abates_class_CF

graph_abates_class_GR <- Freq_GR %>%
  plot_ly(x = ~n, y = ~cat, type = 'bar') %>%
  layout(title = "Classificacao da carcaca") %>%
  layout(xaxis = list(title = "Frequencia"), yaxis = list(title = "Classificacao GR"))
graph_abates_class_GR

# Analise estatistica das variaveis
# Sao todas variaveis qualitativas, portanto vai tudo corrido a teste chi quadrado

x_Raca_abate<- chisq.test(abates1$Raca, abates1$Tipo_abate, simulate.p.value = TRUE) #<0.05 portanto significancia
x_Raca_CT<-chisq.test(abates1$Raca, abates1$MMA_CLA_CAR_CT, simulate.p.value = TRUE)#<0.05 portanto significancia
x_Raca_CF<-chisq.test(abates1$Raca, abates1$MMA_CLA_CAR_CF, simulate.p.value = TRUE)#<0.05 portanto significancia
x_Raca_GR<-chisq.test(abates1$Raca, abates1$MMA_CLA_CAR_GR, simulate.p.value = TRUE)#<0.05 portanto significancia

