library(tidyverse)
library(data.table)
library(lubridate)
library(freqtables)

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
    Peso = as.numeric(str_replace(Peso, ",", "."))
    )

# A nivel de estatistica descritiva por raça
Abates_prop <- Abates_peso %>% 
  group_by(Raca) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

# Tabela freq 
Freq_abates <- Abates_peso %>% 
  freq_table(Raca)

