setwd("C:/Users/ASUS/Desktop/Epi/Problema 4 Abates/Abates/AbatesOrganizacao")
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
#Graph_M_Y <- Peso_by_Matadouro_Year %>% 
  #ggplot(aes(fill=Data, x=Matadouro, y=Avg)) +
  #geom_bar(position = "dodge", stat = "identity") +
  #ggtitle("Média Peso/Abates por Matadouro 2013 - 2017") +
  #xlab("Matadouro") + ylab("Média de Peso")

#Graph_M_Y

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

Abates_by_Time$Data_abate <- as.Date(Abates_by_Time$Data_abate)
#install.packages("hydroTSM")
library(hydroTSM)

Abates_by_Time$Season <- time2season(Abates_by_Time$Data_abate,
                                     out.fmt = "seasons")

Abates_by_Season <- Abates_by_Time %>% 
  group_by(Season) %>% 
  summarise(numero_abates_data = n())

Abates_by_Season

# Passando para o segundo objetivo
# Relacionar abates com a exportação do INE

Dados_exportações <- fread("./Exportacao 12-18.csv")
Dados_exportações <- Dados_exportações[-c(1,2,4,5,6,15:47)] 

#Data cleaning

Dados_exportações <- as.data.frame(t(Dados_exportações)) 
Dados_exportações <- Dados_exportações[,-2, -1]
Dados_exportações <- Dados_exportações[,-4, -5]
Dados_exportações <- Dados_exportações[,-1]
Dados_exportações <- Dados_exportações[,-2, -3]
Dados_exportações <- Dados_exportações[,-2]

colnames(Dados_exportações)[1] <- "Year" 
colnames(Dados_exportações)[2] <- "Total de Exportações (Euro)" 
colnames(Dados_exportações)[3] <- "Intra EU (Euro)"
colnames(Dados_exportações)[4] <- "Extra EU (Euro)" 

Dados_exportações <- Dados_exportações[-c(1,2),]
Dados_exportações$Year <- as.numeric(Dados_exportações$Year)
Dados_exportações$`Total de Exportações (Euro)` <- as.numeric(Dados_exportações$`Total de Exportações (Euro)`)

print(Dados_exportações)

Correlation_Abate_Export <- full_join(Abates_by_Time, Dados_exportações, by = "Year")

Abates_by_Year_Sum <- aggregate(Correlation_Abate_Export["numero_abates_data"], by = Correlation_Abate_Export["Year"], sum)

Abates_by_Year_Sum_Export <- left_join(Abates_by_Year_Sum, Dados_exportações) %>% 
  as.data.frame()
Abates_by_Year_Sum_Export<- Abates_by_Year_Sum_Export[-6,]
#install.packages("ggpubr")
library("ggpubr")

ggqqplot(Abates_by_Year_Sum_Export$`Total de Exportações (Euro)`, ylab = "Eur")
ggqqplot(Abates_by_Year_Sum_Export$numero_abates_data, ylab = "N")

Pearson_Test <- cor.test(Abates_by_Year_Sum_Export$`Total de Exportações (Euro)`, Abates_by_Year_Sum_Export$numero_abates_data,
                         method = "pearson")

Pearson_Test
