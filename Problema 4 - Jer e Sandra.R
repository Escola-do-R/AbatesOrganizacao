library(tidyverse)
library(data.table)

#Nº de abates por mês/ano e kg por mês/ano por matadouro e exploração. 
#Ver a evolução ao longo do tempo;

Abates <- fread("./Abates.csv") 
Abates
Abates <- select(Abates,Data_abate,Matadouro,Exploracao,Peso)
Abates_numero <- select(Abates,Data_abate,Matadouro,Exploracao)
Abates_numero_por_data <- arrange(Abates_numero,Data_abate)
#Próximo passo dividir a data em três colunas