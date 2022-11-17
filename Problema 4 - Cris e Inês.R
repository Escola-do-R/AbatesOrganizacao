library(tidyverse)
library(data.table)

Abates <- fread("./Abates.csv") 
Abates

Abates1 <- select(Abates, Matadouro, MAC, Tipo_abate, Exploracao, Raca)
