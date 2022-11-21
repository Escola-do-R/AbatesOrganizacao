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
##Descrição das variáveis
#Peso: variável quantitativa contínua
#Sexo: variável qualitativa nominal (binomial)
#Raça: variável qualitativa nominal
#Idade: variável quantitativa contínua

# Assim, vou selecionar as colunas que me interessam e garantir que sao da classe certa
# Calculei idade ao abate usando funcao do lubridate %--% e arredondei a 1 casa decimal
# Peso convertido a numerico, necessario substituir as , por .
Abates_peso <- select(Abates, Data_abate, Data_nasc, Peso, Raca, Sexo) %>% 
  mutate(
    idade_ao_abate= round ((Data_nasc %--% Data_abate) / years(1),1),
    Peso = as.numeric(str_replace(Peso, ",", ".")),
    ) %>% 
  mutate(Raca = (str_replace(Raca,c("<",">"),"")))

##ESTATÍSTICA DESCRITIVA
# Acho que repeti as tabelas de frequencia, mas if anything temos 2 maneiras possiveis de as fazer
# Fiz à brute force e com library freqtables

#VARIÁVEIS QUALITATIVAS 
# Raca
Abates_prop_raca <- Abates_peso %>% 
  group_by(Raca) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

Freq_abates_raca <- Abates_peso %>% 
  freq_table(Raca)

# Sexo
Abates_prop_sexo <- Abates_peso %>% 
  group_by(Sexo) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

Freq_abates_sexo <- Abates_peso %>% 
  freq_table(Sexo)


#VARIÁVEIS QUANTITATIVAS CONTÍNUAS
# Idade
stats_idade <- Abates_peso %>% 
  get_summary_stats(idade_ao_abate)

Abates_peso$idade_range <- cut(Abates_peso$idade_ao_abate, breaks= c(0,1,2,3,4,5,10,20,30),
                               labels=c("0-1","1-2","2-3","3-4","4-5","5-10","10-20","+20"))

Abates_prop_idade <- Abates_peso %>% 
  group_by(idade_range) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))

Freq_abates_idade <- Abates_peso %>% 
  freq_table(idade_range)


#Peso
stats_peso <- Abates_peso %>% 
  get_summary_stats(Peso)

Abates_peso$peso_range <- cut(Abates_peso$Peso, breaks= c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,800),
                              labels=c("0-50","50-100","100-150","150-200","200-250","250-300","300-350","350-400", "400-450", "450-500", "500-550", "550-600", "600-650", "650-700", "+700"))

Abates_prop_peso <- Abates_peso %>% 
  group_by(peso_range) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion = n / sum(n))%>% 
  mutate(Percent = (n / sum(n) * 100) %>% round(3))
#ou
Freq_abates_peso <- Abates_peso %>% 
  freq_table(peso_range) %>%
  mutate(Percent = (n / sum(n) * 100) %>% round(3)) #a diferença está nos arrendondamentos e no facto de que nao inclui NA's



# REPRESENTAÇÃO GRÁFICA DA ANÁLISE DESCRITIVA
 
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

#grafico plotly para as ranges de peso
graph_abates_peso <- Abates_peso %>% 
  plot_ly(x=~peso_range, type="histogram") %>%
  layout(title="Abates por Peso") %>%
  layout(xaxis = list(title = "Peso"), yaxis = list(title = "Frequência"))
graph_abates_peso


##TENTATIVA DE ANÁLISE
#BOXPLOTS
#para comparar a distribuição dos pesos tendo em conta o sexo, raça e idade. Falta melhorar a estética/labels
box_peso_sexo <- Abates_peso %>%
  plot_ly(y=~Peso, x=~Sexo, type="box")
box_peso_sexo 
  
box_peso_raca <- Abates_peso %>%
  plot_ly(y=~Raca, x=~Peso, type="box")
box_peso_raca  
  
box_peso_idade <- Abates_peso %>%
  plot_ly(y=~idade_range, x=~Peso, type="box")   
box_peso_idade


###PESO-IDADE (quantitativa-quantitativa)

##testar permissas da ANOVA
#1. Homogeneidade - Levene Test testa se a variancia entre grupos é igual. H0=variancia igual; H1=variancia NAO igual
library(car)
leveneTest(Peso ~ idade_range, data=Abates_peso) #como Pr<0.05, rejeita-se H0, ou seja: variancia NAO é igual (suportada pelo boxplot) e nao se pode usar ANOVA?
#2. testar dist. normal - Kolmogorov-Smirnov Test para n>50. nao faço ideia se isto está bem
ks.test(Abates_peso$idade_ao_abate, Abates_peso$Peso)
#Alternativa teste não paramétrico do One way ANOVA test é o KRUSKAL-WALLIS TEST (e correlaçao de spearman em vez de pearson)
  