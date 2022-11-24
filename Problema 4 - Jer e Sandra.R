library(tidyverse)
library(data.table)
library(data.table)
library(dplyr)
library(ggplot2)

#Nº de abates por mês/ano e kg por mês/ano por matadouro e exploração. 
#Ver a evolução ao longo do tempo;

Abates <- fread("./Abates.csv") 

Abates <- select(Abates,Data_abate,Matadouro,Exploracao,Peso)
Abates_numero <- select(Abates,Data_abate,Matadouro,Exploracao)
Abates_numero_por_data <- arrange(Abates_numero,Data_abate)
#Próximo passo dividir a data em três colunas

Abates_by_Expl <- Abates %>% 
  group_by(Exploracao) %>% 
  summarise(numero_abates = n())

Abates_by_Time <- Abates %>% 
  mutate(Data_abate = as.Date(Data_abate),
         Year_Month = format(Data_abate, "%Y-%m")) %>% 
  group_by(Year_Month) %>% 
  summarise(numero_abates_data = n())

Abates_by_Matadouro <- Abates %>% 
  group_by(Matadouro) %>% 
  summarise(numero_abates = n())