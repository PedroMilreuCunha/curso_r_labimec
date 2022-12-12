# Bibliotecas ----

library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr) #Utilizada para leitura dos dados
library(lubridate) #Utilizada para manipulação de datas
library(MeTo) #Utilizada para estimação da Irradiação Extraterrestre Total

# Leitura dos dados ----

dados <- read_delim("Dados - Finais.csv", ";",
                    escape_double = FALSE, locale = locale(decimal_mark = ","), 
                    trim_ws = TRUE)
dados$TIMESTAMP <- as.POSIXct(dados$TIMESTAMP,
                              format = "%d/%m/%Y %H:%M") 

# Dashboard ----

# (UI) - INTERFACE DO USUÁRIO ----

header <- dashboardHeader(title = "Simulação - Irradiância Solar e Irradiância Solar Extraterrestre para Água Branca (AL)",
                          titleWidth = 1600)  # Desabilitar o cabeçalho do Dashboard

sidebar <- dashboardSidebar(disable = TRUE) # Desabilitar a barra lateral, pois não há nada para colocarmos lá

 # Criação das linhas do Dashboard (organizado como uma matriz)

frow0 <- fluidRow(box(width = 12, solidHeader = TRUE, background = "blue", "Entrada de parâmetros", align = "center"))

frow1 <- fluidRow(# Linha dos inputs
  column(6, align = "center", wellPanel(
    selectInput("data", 
                 label = "Escolha um mês",
                 choices = c("Janeiro" = "01/01",
                             "Fevereiro" = "01/02",
                             "Março" = "01/03",
                             "Abril" = "01/04",
                             "Maio" = "01/05",
                             "Junho" = "01/06",
                             "Julho" = "01/07",
                             "Agosto" = "01/08",
                             "Setembro" = "01/09",
                             "Outubro" = "01/10"),
                 selected = "01/01", width = "400px"))),

  column(6, align = "center", wellPanel(
    sliderInput("intervalo_tempo", 
                label = "Selecione o intervalo de tempo desejado (em horas)",
                value =  c(0,23) , min = 0, max = 23,
                width = "800px")))
)


frow2 <- fluidRow(box(width = 12, solidHeader = TRUE, background = "blue", "Estatísticas descritivas", align = "center"))

frow3 <- fluidRow( 
  valueBoxOutput("valor1"), # Criação de caixas para destacar estatísticas de interesse
  valueBoxOutput("valor2"),
  valueBoxOutput("valor3")
)

frow4 <- fluidRow(title = "Visualização gráfica", # Caixa para envolver o gráfico
                  column(12, align = "center", box(width = 12,  align = "center",
                                                   title = "Visualização gráfica - Irradiância Solar e Irradiância Solar Extraterrestre para o ano de 2008",
                                                   status = "primary",
                                                   solidHeader = TRUE ,
                                                   collapsible = FALSE ,
                                                   footer = "Elaborado por: ",
                                                   plotOutput("irradiancia_plot", height = "450px", width = "1000px")))
)



# Combinando as linhas para criar o corpo da dashboard
body <- dashboardBody(frow0, frow1, frow2, frow3, frow4)

# Finalizando a criação da User Interface (UI) com a dashboardPage
ui <- dashboardPage(title = "Simulação - Irradiância Solar e Irradiância Solar Extraterrestre para Água Branca (AL)",
                    header, sidebar, body, skin='black') # 1º componente necessário

# SERVIDOR ----
server <- function(input, output) { # 2º componente necessário

  # Criação do output das caixas

  output$valor1 <- renderValueBox({
    
    minimo <- min(subset(dados, Data == input$data & `Hora local` >= input$intervalo_tempo[1] & 
                           `Hora local` <= input$intervalo_tempo[2])$RadHZtot_Avg)
    
    valueBox(paste(formatC(minimo, format = "f", digits = 2), "W/m²"),
             subtitle = paste('Irradiância total mínima para o dia', input$data, "de 2008"),
             color = "green")
})
  
  output$valor2 <- renderValueBox({
    
    media <- mean(subset(dados, Data == input$data & `Hora local` >= input$intervalo_tempo[1] & 
                                `Hora local` <= input$intervalo_tempo[2])$RadHZtot_Avg)                               
    
    valueBox(paste(formatC(media, format = "f", digits = 2), "W/m²"),
                           subtitle = paste('Irradiância total média para o dia', input$data, "de 2008"),
                           color = "purple")
})
  
  output$valor3 <- renderValueBox({
    
    maximo <- max(subset(dados, Data == input$data & `Hora local` >= input$intervalo_tempo[1] & 
                                `Hora local` <= input$intervalo_tempo[2])$RadHZtot_Avg)
    
                                    valueBox(paste(formatC(maximo, format = "f", digits = 2), "W/m²"),
                                             subtitle = paste('Irradiância total máxima para o dia', input$data, "de 2008"),
                                             color = "navy")
})
  
  #Criação do output do gráfico
  
  output$irradiancia_plot <- renderPlot({
    
    #Cálculos para estimar a Irradiância Extra Terrestre
  
    temp <- subset(dados, Data == input$data & `Hora local` >= input$intervalo_tempo[1] & 
                     `Hora local` <= input$intervalo_tempo[2])
    
    temp$estimado <- Ra(temp$TIMESTAMP, lat.deg = -9.25, long.deg = 37.93,
                         control = list(lz = 0))*(1000000/60)
    # A função Ra calcula a irradiação extraterrestre à partir da data, da latitude e da longitude.
    # O resultado é expresso em MJ então é preciso multiplicar por 1000000/60 para obter a medida em W/m².
    
    cols <- c("Medido"="red","Extra Terrestre (estimado)"="blue")
    line_types <- c("Medido"=1,"Extra Terrestre (estimado)"=1)
    
    # Criação do gráfico em si
    
    ggplot(data = temp,
           aes(x=`Minuto do dia`, y=RadHZtot_Avg)) + 
    geom_line(aes(color = "Medido", linetype = "Medido"), size = 1) + 
    geom_line(aes(y = estimado, color = "Extra Terrestre (estimado)", linetype = "Extra Terrestre (estimado)"),
              size = 1.25) +
    scale_colour_manual(name="Variável", values=cols, 
                        guide = guide_legend(override.aes=aes(fill=NA))) + 
    scale_linetype_manual(name = "Variável", values=line_types) + 
    scale_x_continuous(n.breaks = 30) +
    scale_y_continuous(n.breaks = 10) +
    labs(x = "Minuto do dia", y = "Valor (W/m²)",
         title = paste("Irradiância Solar ao longo do dia", input$data, "de 2008")) +
    theme_classic() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          legend.text = element_text(color = "black", size = 12),
          legend.title = element_text(color = "black", size = 12, face = "bold"),
          legend.position = "top")
  })

}

shinyApp(ui, server)
