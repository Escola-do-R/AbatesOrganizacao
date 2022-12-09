setwd("C:/Users/ASUS/Desktop/Epi/Problema 4 Abates/Abates/Abates Organização")
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(xts)


#
Abates <- fread("./Abates.csv") 
Abates[, 7] <- as.numeric(as.character(sub("," , ".", Abates$Peso)))
#

#Nº de abates por mês/ano
Abates_by_Time <- Abates %>% 
  group_by(Data_abate) %>% 
  summarise(numero_abates_data = n()) %>% 
  as.data.frame() 

Abates_by_Time <- Abates_by_Time %>% 
  mutate(Day = day(Data_abate), Month = month(Data_abate), Year = year(Data_abate)) 

Stats <- Abates_by_Time %>%
  group_by(Year,Month) %>% 
  summarise(Avg = mean(numero_abates_data))

#Using Yearmon we retain the class of date. 
Stats$Data <- as.yearmon(paste(Stats$Year, Stats$Month), 
                         "%Y %m")

#Ver a evolução ao longo do tempo;
Graph <- Stats %>% 
  ggplot(aes(Data, Avg)) +
  geom_point(na.rm=TRUE, color="purple", size=1) +
  ggtitle("Evolução de Abates 2013 - 2017") +
  xlab("Data") + ylab("Abates") +
  geom_smooth(method='lm')

Graph

#kg por mês/ano por matadouro 
#Abates por Matadouro 2013-2017
Abates_by_Matadouro <- Abates %>% 
  group_by(Matadouro) %>% 
  summarise(numero_abates = n())

Peso_by_Matadouro_Year <- Abates %>% 
  mutate(Month = month(Data_abate), Year = year(Data_abate)) %>% 
  select(Matadouro, Peso, Month, Year) 
   
Peso_by_Matadouro_Year$Data <- as.yearmon(paste(Peso_by_Matadouro_Year$Year, 
                                                Peso_by_Matadouro_Year$Month), "%Y %m")

Peso_by_Matadouro_Year <- Peso_by_Matadouro_Year %>% 
  group_by(Matadouro, Data) %>% 
  summarise(Avg = mean(Peso)) %>% 
  na.exclude()

#Visualization
#Dont work, will come back to it e VAMOS IGNORAR ;)
Graph_M_Y <- Peso_by_Matadouro_Year %>% 
  ggplot(aes(fill=Data, x=Matadouro, y=Avg)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Média Peso/Abates por Matadouro 2013 - 2017") +
  xlab("Matadouro") + ylab("Média de Peso")

Graph_M_Y

#kg por mês/ano por exploração. 
Abates_by_Expl <- Abates %>% 
  group_by(Exploracao) %>% 
  summarise(numero_abates = n())

Peso_by_Expl_Year <- Abates %>% 
  mutate(Month = month(Data_abate), Year = year(Data_abate)) %>% 
  select(Exploracao, Peso, Month, Year) 

Peso_by_Expl_Year$Data <- as.yearmon(paste(Peso_by_Expl_Year$Year, Peso_by_Expl_Year$Month),
                                          "%Y %m")

Peso_by_Expl_Year <- Peso_by_Expl_Year %>% 
  group_by(Exploracao, Data) %>% 
  summarise(Avg = mean(Peso)) 

#Ver se há Sazonalidade nos abates
# Inverno: 21/12 - 20/03
# Primavera: 21/03 - 20/06
# Verão: 21/06 - 20/09
# Outono: 21/09 - 20/12 

# Agrupamento por mês e dia
Stats1 <- Abates_by_Time %>%
  group_by(Month,Day,Year) %>% 
  summarise(Avg = mean(numero_abates_data))

ggplot(Stats1, aes(x=factor(Month), y=Avg))+
  geom_boxplot()+
  ylab('Média de abates')+
  xlab('Mês')+
  theme_bw()
  
# Está um gráfico e dá para ver quando há mais abates, podia estar pior ahahah Mas ainda temos de ver se há uma melhor maneira de representar
