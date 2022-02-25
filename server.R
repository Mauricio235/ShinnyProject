

library(shiny)
library(rsconnect)
library(shiny)
library(plotly)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(runner)
library(dplyr)


# Define server logic required to draw a histogram
server <- function(input, output){
  
  #Plotando o grafico de linhas 
  output$line<- renderPlotly({
    
    baseFinal_input1<- baseFinal%>% filter(baseFinal$Ano== input$checkGroup )
    avalia_crescimento<-aggregate(baseFinal_input1$clientes_ativos, 
                                  by=list(month=baseFinal_input1$month), FUN=sum)
    avalia_crescimento$mes <- substr(avalia_crescimento$month, 6,7)
    avalia_crescimento$Ano <- substr(avalia_crescimento$month, 1,4)
    ggplotly(plot1_line(avalia_crescimento))  
  })

  #Plotando o grafico de linhas por franquias
  output$lineFranquia<- renderPlotly({

    baseFinal_input2<- baseFinal%>% filter(baseFinal$Ano== input$checkGroup & baseFinal$franquia==input$variable)
    avalia_crescimento1<-aggregate(baseFinal_input2$clientes_ativos,
                                  by=list(month=baseFinal_input2$month), FUN=sum)
    avalia_crescimento1$mes <- substr(avalia_crescimento1$month, 6,7)
    avalia_crescimento1$Ano <- substr(avalia_crescimento1$month, 1,4)
    ggplotly(plot2_line(avalia_crescimento1))
  })

  
  #plotando o boxplot
  output$plot<- renderPlotly({
    
    baseFinal_input1<- baseFinal%>% filter(baseFinal$Ano== input$checkGroup)
    avalia_crescimento<-aggregate(baseFinal_input1$clientes_ativos, 
                                  by=list(month=baseFinal_input1$month), FUN=sum)
    avalia_crescimento$mes <- substr(avalia_crescimento$month, 6,7)
    avalia_crescimento$Ano <- substr(avalia_crescimento$month, 1,4)
    
    
    boxplot1<-plot_boxplot(avalia_crescimento)
    ggplotly(boxplot1)
    
  })
  
  
  #Inserindo valor a primeira caixa
  output$Totalclientes <- renderValueBox({
    baseFinal_input1<- baseFinal%>% filter(baseFinal$Ano== input$checkGroup )
    clientes<- sum(baseFinal_input1$clientes_ativos)
    valueBox(
      paste0(round(clientes, digits = 2)),
      subtitle = "Clientes Ativos no último mês", color = "green"
    )
  })
  
  #Inserindo valor a primeira caixa
  output$Totalreceita1 <- renderValueBox({
    baseFinal_input1<- baseFinal%>% filter(baseFinal$Ano== input$checkGroup )
    clientes1<- sd(baseFinal_input1$clientes_ativos)
    valueBox(
      paste0(round(clientes1, digits = 2)),
      subtitle = "Desvio padrão do numero de clientes", color = "green"
    )
  })
  
  
  #Plotando a tabela de indices 
  output$table1<- renderDataTable({
    avalia_indice
  })
  
  
  #Plotando a estrutura da base de dados
  #head of data
  output$HeadData <- renderDataTable({
    baseFinal_input1<- baseFinal%>% filter(baseFinal$Ano== input$checkGroup)
    head(baseFinal_input1[,1:11])

  })
  
  output$code<-  renderPrint({ 
    str(baseFinal)
  })
  
  
  #Caixa de seleção de franquias

  

  #Renderizando as medidas descritivas 
  output$summary<- renderPrint({
    summary(baseFinal)
  })
  
  #Renderizando o teste qui quadrado de associação
  output$testresult<- renderPrint({
    chisq.test(baseFinal$regiao, baseFinal$tipo)
    
  })
  
  
  #Renderizando o teste de correlação
  output$testresultado<- renderPrint({
    cor(baseFinal$clientes_ativos, baseFinal$renda_media_familiar)
   
  })
  
  output$descTest<- renderText({
    "Pela correlação ao lado, é fácil ver que a relação entre renda media 
    familiar e número de clientes ativos é bem fraca e próxima de zero, 
    assumindo o valor de -0,075. Isso representa um bom sinal devido ao fato de 
    que não há tanta diferença entre as rendas médias quando tomadas por 
    região, e esse valor interfere de forma não tão forte com a variável 
    clientes ativos."
  })
  
  
  output$descriptionTest<- renderText({
     "O teste qui quadrado de associação, foi realizado para comprovar 
     a existência de mais áreas rurais em algumas regiões. Esse teste 
     deve ser aplicado em variáveis categóricas para avaliar a 
     independência. As hipóteses do teste são:
    
    H0: Existe independência entre as variáveis
    H1: As variáveis não são independentes.
    
    Com um p-valor<0,05, ao nível de 5% de significância rejeita-se 
    que há independência entre essas variáveis."
  })
  
  #Resultados do boxplot
  output$text1 <- renderText({
    "    Os dados dos índices foram representados tomando-se o logaritmo para facilitar 
    a visualização dos dados, haja vista que o resultado não é alterado devido à função 
    logaritmo ser bijetora e monotonamente crescente.\n    De acordo com o boxplot e a tabela ao
    lado, é possível verificar que o maior índice médio de clientes por população residente
    se encontra na região Geo NE, com aproximadamente 60 clientes a cada 1000 habitantes. Com isso,
    representa a região com o maior potencial até então, pois possui mais cliente por habitante 
    residente. Em seguida tem-se a região Geo NO com apriximadamente 32 clientes ativos a cada 
    1000 habitantes, representando a segunda região com maior potencial."
  })
  
  
  output$results1<- renderText({
    "Para visualização do crescimento, o gráfico de linhas representa 
    uma comparação por ano e mês. É notório que no ano de 2021, o número
    total de clientes se manteve estável enquanto que no ano de 2019 teve um aumento gradativo.
    A comparação por franquia permite observar a situação de cada uma delas 
    nesse período registrado. É possível ver que em algumas o número de clientes
    ativos caiu quando comparado em 2019 e em 2021. É importante que essa análise seja acompanhada de 
    ações que manterão essas franquias com queda em seu desenvolvimento em alta. 
    Como estratégia para o controle do número de clientes ativos, um modelo de previsão dessa variável pode 
    evitar possíveis dumps repentinos no crescimento dessas franquias, tendo como variável resposta o número 
    de clientes."
  })
  
  output$informacao<- renderText({
    "--> A guia dashboard contém as principais medidas e visualizações. Para 
    analisar o crescimento do número de clientes ativos por franquia e mês, altere 
    no gráfico a aba de visualização e em seguida escolha a franquia desejada. 
    
    --> Sempre insira dados para análise que  evitarão o problema da ausencia de dados.
    
    --> Escolha o ano desejado na seleção de caixas por ano. Essa visualização somente
    será alterada na guia dashboard. 
    
    --> A guia base de dados, representa medidas descritivas da base de dados, bem como
    a base final composta pelas bases franquias e demograficas. A base franquias continha
    informações das franquias em geral enquanto que a base demograficas continha o número de 
    habitantes, renda média entre outras. 
    
    --> A guia testes representa algumas observações das variáveis renda, índice, 
    região e tipo. 
    
    "
  })
}


