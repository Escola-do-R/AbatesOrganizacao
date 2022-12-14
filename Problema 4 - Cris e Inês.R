library(tidyverse)
library(data.table)
library(plotly)
library(janitor)
library(RColorBrewer)
library(leaflet)
library(sf)
library(lubridate)
library(measurements)

Abates <- fread("./Abates.csv") 
Abates

Abates1 <- select(Abates, Matadouro, MAC, Tipo_abate, Exploracao, Raca) %>%  
  mutate(Matadouro = (str_replace(Matadouro,c("<",">"),""))) %>%
  mutate(Raca = (str_replace(Raca,c("<",">"),"")))

# Grafico da percentagem de tipo de abate
graph_tipoabate <- plot_ly(data = count(Abates1, Tipo_abate), 
                      labels = ~Tipo_abate, values = ~n, type = "pie") %>% 
              layout(title = "Tipo de Abates")

graph_tipoabate
# # # # # # #

# Grafico de barras do total de abates por matadouro
abates_matadouro <- summarize(
    group_by(Abates1, Matadouro), 
    count=n()
  )

graph_abates_matadouro <- abates_matadouro %>% 
  plot_ly(x = ~count, y = ~Matadouro, type = 'bar') %>% 
  layout(title = "Abates por Matadouro")

graph_abates_matadouro
# # # # # # # # # #

# Grafico tipo de abate por matadouro
tipoabate_matadouro <- tabyl(Abates1, Matadouro, Tipo_abate)

graph_tipoabate_matadouro <- tipoabate_matadouro %>% 
  plot_ly(x = ~E, y = ~Matadouro, type = 'bar', name = 'Emergencia') %>% 
  add_trace(x = ~N, name = 'Normal') %>% 
  add_trace(x = ~S, name = 'Sanitario') %>% 
  layout(title = "Tipo de Abate por Matadouro", barmode = 'group')

graph_tipoabate_matadouro

# Grafico abates por matadouro por raca

# abates_matadouro_raca <- tabyl(Abates1, Matadouro, Raca)
# tabela_count_raca <- left_join(abates_matadouro, abates_matadouro_raca)

tab_raca_matadouro <- group_by(Abates1, Matadouro) %>% count(Raca)

# tab_raca_matadouro <- tab_raca_matadouro %>% mutate(Raca = (str_replace(Raca,c("<",">"),"")))

# Criar palete para as nossas 38 racas, porque as paletes pre definidas nao suportam tantas classes
pal <- brewer.pal(12, "Set3") 
pal <- colorRampPalette(pal)(38)

graph_abates_matadouro_raca <- plot_ly(data = tab_raca_matadouro, x = ~Matadouro, y = ~n,
                                       color = ~Raca, colors = pal,
                                       type = 'scatter', mode = 'markers')

graph_abates_matadouro_raca
 
# Chi square independence test

tab_raca_tipoabate <- tabyl(Abates1, Raca, Tipo_abate)

# chisq_raca_tipoabate <- chisq.test(tab_raca_tipoabate)
# chisq_raca_tipoabate
# Temos aviso de aproximacao incorreta e isso tem a ver com freq inferior a 5(?)

# Para contornar podemos usar o Fisher exact test

# fisher_raca_tipoabate <- fisher.test(tab_raca_tipoabate)
# Error in stats::fisher.test(., ...) : FEXACT error 40. Out of workspace. 
# Nao encontro respostas a isto online

# Exploracao com o tipo de abate

tab_exp_tipoabate <- tabyl(Abates1, Exploracao, Tipo_abate)
tab_exp_tipoabate2 <- select(Abates1, Exploracao, Tipo_abate)

# chisq_exp_tipoabate <- chisq.test(tab_exp_tipoabate)
# chisq_exp_tipoabate

# fisher_exp_tipoabate <- fisher.test(tab_exp_tipoabate2)
# Error in stats::fisher.test(., ...) : FEXACT error 40.Out of workspace.

# mapa matadouro exploracoes #
# ficheiros
mapa_continente <- st_read("Cont_AAD_CAOP2020")
mapa_continente$geometry <- st_transform(mapa_continente$geometry, "+init=epsg:4326")

Codme <- fread("./Cod_ME_DiCo.csv") %>% unique
Total_Caract_Expl <- fread("./FicheiroTotalCaracterizacaoExploracoes-2022-10-04.csv")
Freguesias <- fread("./Correspond??ncias freg 2013-14.csv")
# # # # # # # # # # # # # # # # # # # # # # # # 

Abates2 <- select(Abates1, Matadouro, Exploracao)
names(Abates2)[names(Abates2) == 'Exploracao'] <- 'ME'
Abates2 <- mutate(Abates2, ME = paste("PT", Abates2$ME, sep = ''))

SantaCarnes <- Abates2[Abates2$Matadouro == "SANTACARNES - COMRC?? E INDST?? DE CARNES DE SANTAR??M, SA"]
Raporal <- Abates2[Abates2$Matadouro == "RAPORAL - RA????ES DE PORTUGAL, SA"]
RegMafra <- Abates2[Abates2$Matadouro == "MATADOURO REGIONAL MAFRA"]

Codme1 <- select(Codme, ME, DiCoFre)
Codme1 <- mutate(Codme1, ME = paste("PT", Codme1$ME, sep = '')) #Don't run twice!!

Freguesias1 <- select(Freguesias, DiCoFre, `Freguesias - Reorganiza????????o de 2013`)

# limpar tabela caracterizacao

Total_Caract_Expl <- Total_Caract_Expl %>% arrange(desc(DAT_ALT)) %>% distinct(CEX_MAR_EXP, .keep_all = TRUE)

Total_Caract_Expl <- unite(Total_Caract_Expl, "DiCoFre", CEX_COD_DIS, CEX_COD_CON, CEX_COD_FRE, sep = "")

Total_Caract_Expl <- select(Total_Caract_Expl, CEX_MAR_EXP, DiCoFre) %>% unique

names(Total_Caract_Expl)[names(Total_Caract_Expl) == 'CEX_MAR_EXP'] <- 'ME'
# # # # # # # # # # # # # #

Dados_Exp <- full_join(Codme1, Total_Caract_Expl) %>% unique
Dados_Exp <- left_join(Dados_Exp, Freguesias1)

# SantaCarnes -----------------------------------------------
SantaCarnes <- group_by(SantaCarnes, ME) %>% count(ME)

SantaCarnes1 <- left_join(SantaCarnes, Dados_Exp) %>% filter(nchar(DiCoFre) == 6)

mapa_SantaCarnes <- sp::merge(mapa_continente,SantaCarnes1, by.x="Dicofre", by.y="DiCoFre")

# cores
bins_SantaCarnes <- c(0, 10, 40, 100, 500, 1000, 2000, 4000, Inf)
pal_SantaCarnes <- colorBin("Greens",mapa_SantaCarnes$n, bins_SantaCarnes) 

# texto
mytext_SantaCarnes <- paste(
  "<strong>", "Freguesia: ", "</strong>", mapa_SantaCarnes$`Freguesias - Reorganiza????????o de 2013`, "<br/>",
  "<strong>", "Animais: ", "</strong>", mapa_SantaCarnes$n, "<br/>") %>%
  lapply(htmltools::HTML)


# mapa 

SantaCarnes_leaflet <- leaflet(data = mapa_SantaCarnes) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, fillColor = ~pal_SantaCarnes(n), fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_SantaCarnes, labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_SantaCarnes, values = ~n, opacity = 0.7, title = "Animais Abatidos",
            position = "bottomright")

SantaCarnes_leaflet

# -----------------------------------------------------------

# Raporal ---------------------------------------------------
Raporal <- group_by(Raporal, ME) %>% count(ME)

Raporal1 <- left_join(Raporal, Dados_Exp) %>% filter(nchar(DiCoFre) == 6)

mapa_Raporal <- sp::merge(mapa_continente,Raporal1, by.x="Dicofre", by.y="DiCoFre")

# cores
bins_Raporal <- c(0, 10, 40, 100, 500, 1000, 2000, 4000, Inf)
pal_Raporal <- colorBin("Greens",mapa_Raporal$n, bins_Raporal) 

# texto
mytext_Raporal <- paste(
  "<strong>", "Freguesia: ", "</strong>", mapa_Raporal$`Freguesias - Reorganiza????????o de 2013`, "<br/>",
  "<strong>", "Animais: ", "</strong>", mapa_Raporal$n, "<br/>") %>%
  lapply(htmltools::HTML)


# mapa 

Raporal_leaflet <- leaflet(data = mapa_Raporal) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, fillColor = ~pal_Raporal(n), fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_Raporal, labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_Raporal, values = ~n, opacity = 0.7, title = "Animais Abatidos",
            position = "bottomright")

Raporal_leaflet
# -----------------------------------------------------------

# Regional Mafra --------------------------------------------
RegMafra <- group_by(RegMafra, ME) %>% count(ME)

RegMafra1 <- left_join(RegMafra, Dados_Exp) %>% filter(nchar(DiCoFre) == 6)

mapa_RegMafra <- sp::merge(mapa_continente,RegMafra1, by.x="Dicofre", by.y="DiCoFre")

# cores
bins_RegMafra <- c(0, 10, 40, 100, 500, 1000, 1500, 2000, Inf)
pal_RegMafra <- colorBin("Greens",mapa_RegMafra$n, bins_RegMafra) 

# texto
mytext_RegMafra <- paste(
  "<strong>", "Freguesia: ", "</strong>", mapa_RegMafra$`Freguesias - Reorganiza????????o de 2013`, "<br/>",
  "<strong>", "Animais: ", "</strong>", mapa_RegMafra$n, "<br/>") %>%
  lapply(htmltools::HTML)


# mapa 

RegMafra_leaflet <- leaflet(data = mapa_RegMafra) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, fillColor = ~pal_RegMafra(n), fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_RegMafra, labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_RegMafra, values = ~n, opacity = 0.7, title = "Animais Abatidos",
            position = "bottomright")

RegMafra_leaflet

# ------------------------------------------------
