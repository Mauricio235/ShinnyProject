library(rsconnect)
library(readxl)
library(ggplot2)
library(viridis)
library(lubridate)
library(dplyr)
library(plotly)
library(geosphere)
library(sf)
library(reshape2)
library(hrbrthemes)


#Lendo as bases de dados
base_franquias <- read_excel("base_franquias.xlsx")

demograficas <- read_excel("demograficas.xlsx")

#Estatísticas descritivas
summary(base_franquias)
summary(demograficas)
str(base_franquias)
str(demograficas)


#Juntando a base franquias com demograficas
baseFinal<-merge(base_franquias, demograficas, by = "cidade")


#Como o estudo e direcionado ao numero de clientes ativos, não faz sentido estudar 
#os registros que não possuem esse dado. Porém é muito importante verificar o por que 
#estão ausentes esses dados juntamente com a fonte que originou os dados. 
baseFinal<-na.omit(baseFinal)


#Criando uma nova coluna com o valor do indice por franquia observada
baseFinal <-baseFinal %>% 
  mutate( baseFinal$clientes_ativos/baseFinal$soma_pop_total)

colnames(baseFinal)[15] <- c("Indice")





#Agregando os dados por regiao para avaliação do indice
avalia_indice<-aggregate(baseFinal$Indice, 
                         by=list(Category=baseFinal$regiao), FUN=mean)


#Criando a função para plotar o Boxplot para avaliar o indice por regiao
plot_boxplot<-function(x){
  ggplot(baseFinal) +
    aes(x = regiao, y = log(Indice), fill = regiao) +
    geom_boxplot() +
    labs(x= "Região", y= "Índice Médio", title = "Indice Medio por região", 
         subtitle = "Indice médio em escala logarítmica")+
    scale_fill_hue(direction = 1) +
    theme_minimal()
}



#Criando uma nova coluna com os anos
baseFinal$Ano <- format(baseFinal$referencia,"%Y")


#Grafico de linhas 
#round dates down to week
baseFinal$month <- floor_date(baseFinal$referencia, "month")



#Criando uma função para plotar o grafico de crescimento total por ano, mês a mês
plot1_line<-function(avalia_crescimento){
  
  avalia_crescimento %>% 
    ggplot(aes(x=mes, y  =x, color = Ano, group =Ano))+
    geom_line()+
    scale_color_manual(values=c('green4','deepskyblue4'))+
    labs(x="Meses", y="Clientes Ativos", title = "Total de clientes ativos por ano")+
    geom_point()
  
}


#Criando uma função para plotar o grafico de crescimento total por ano, mês a mês
plot2_line<-function(avalia_crescimento1){
  
  avalia_crescimento1 %>% 
    ggplot(aes(x=mes, y  =x, color = Ano, group =Ano))+
    geom_line()+
    scale_color_manual(values=c('green4','deepskyblue4'))+
    labs(x="Meses", y="Clientes Ativos", title = "Total de clientes ativos por ano e franquia")+
    geom_point()
  
}


#De acordo com o grafico pode-se perceber que a região Geo NE apresentou maior indice
#medio, levando em consideração sua população. 


# #teste qui-quadrado de independencia
chisq.test(baseFinal$tipo, baseFinal$regiao)

## Teste de correlação de Pearson
cor.test(baseFinal$renda_media_familiar, baseFinal$Indice)



hist(baseFinal$clientes_ativos)

source("ui.R")
source("server.R")


shinyApp(ui=ui, server=server)





